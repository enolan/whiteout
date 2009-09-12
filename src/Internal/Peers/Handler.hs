module Internal.Peers.Handler
    (
    PeerSt(..),
    connectToPeer,
    peerListener
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Array.IArray (bounds)
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import Data.Int (Int64)
import Data.Iteratee.Base as Iter
import Data.Iteratee.Binary
import Data.Iteratee.WrappedByteString
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Network.Socket hiding (Debug) -- clashes with the LogLevel
import qualified Network.Socket.ByteString as SB
import qualified Network.Socket.ByteString.Lazy as SBL
import qualified Text.Show.ByteString as SBS

import Internal.Logging (maybeLog)
import Internal.Peers.Handler.Messages
import Internal.Pieces
import Internal.Types

data PeerSt = PeerSt {
    connectedPeerSt :: Maybe ConnectedPeerSt, -- ^ Nothing before handshaking
    pSockAddr :: SockAddr -- ^ The address and port of the peer in question.
    }

fromConnectedPeerSt :: PeerSt -> ConnectedPeerSt
fromConnectedPeerSt = fromJust . connectedPeerSt

-- | The state associated with a fully-handshaked peer connection. Used for
-- communication between the reader thread, the writer thread and the peer
-- manager.
data ConnectedPeerSt = ConnectedPeerSt {
    peerId :: B.ByteString,
    interested :: TVar Bool

    -- Later we'll have a TChan of the have messages to send, dupTChan'd from
    -- the global one, and a bitfield, and track choke/interest state here.
    }

-- | Connect to a new peer.
connectToPeer ::
    Session -> TorrentSt -> SockAddr -> IO ()
connectToPeer sess torst sockAddr =
    (block . forkIO $ catches go handlers) >> return ()
    where
        go = bracket
            (do
                s <- socket AF_INET Stream 0
                tid <- myThreadId
                atomically $ do
                    activity <- readTVar . sActivity $ torst
                    case activity of
                        Running -> return ()
                        _ -> error
                            "Torrent stopped, new connection dying."
                    wereAlreadyConnected <-
                        F.any ((==sockAddr) . pSockAddr) <$>
                            (readTVar . sPeers $ torst)
                    if wereAlreadyConnected
                        then error
                            "Already connected to this peer in a different \
                            \thread."
                        else return ()
                    let
                        peerSt = PeerSt
                            {connectedPeerSt = Nothing, pSockAddr = sockAddr}
                    modifyTVar (sPeers torst) (M.insert tid peerSt)
                    return (s, peerSt)
                    )
            (\(s, _) -> do
                sClose s
                tid <- myThreadId
                atomically $ modifyTVar (sPeers torst) (M.delete tid))
            (\(s, peerSt) -> do
                maybeLogPeer sess peerSt Low "Connecting"
                connect s $ pSockAddr peerSt
                sendHandshake sess torst s
                theirHandshake <- getHandshake s
                maybeLogPeer sess peerSt Debug $ B.concat
                    ["Got handshake: ", (BC.pack $ show theirHandshake)]
                if hInfoHash theirHandshake /= tInfohash (sTorrent torst)
                    then error "Wrong infohash in outgoing peer connection!"
                    else return ()
                let
                    numPieces = snd $ bounds $ tPieceHashes $ sTorrent torst
                    (quot', rem') = quotRem numPieces 8
                    bitFieldLen =
                        fromIntegral $ if rem' /= 0 then quot'+1 else quot'
                sendPeerMsg s $ Bitfield $ B.replicate bitFieldLen 255
                sendPeerMsg s Unchoke
                interested' <- newTVarIO False
                let
                    cPeerSt = ConnectedPeerSt {
                        peerId = hPeerId theirHandshake,
                        interested = interested'}
                    peerSt' = PeerSt {
                        connectedPeerSt = Just cPeerSt,
                        pSockAddr = pSockAddr peerSt}
                tid <- myThreadId
                atomically $ modifyTVar (sPeers torst) (M.insert tid peerSt')
                -- Overwrites the unconnected PeerSt inserted by getNextPeer.
                peerHandler sess torst peerSt' s
                maybeLogPeer sess peerSt' Debug "Disconnecting (normal).")
        handlers = [
            Handler (\(e :: IOException) -> handleEx e),
            Handler (\(e :: ErrorCall) -> handleEx e),
            Handler (\(e :: AsyncException) -> handleAsync e)
            ]
        handleAsync e = if e == ThreadKilled then handleEx e else throwIO e
        handleEx :: Show e => e -> IO ()
        handleEx e = atomically $ maybeLog sess Medium $ B.concat
            ["Caught exception in peer handler. ",
             BC.pack . show $ sockAddr,
             ": ", BC.pack $ show e]

peerListener :: Session -> PortNumber -> IO ()
peerListener sess p = bracket (socket AF_INET Stream 0) sClose $ \ls -> do
    bindSocket ls $ SockAddrInet p iNADDR_ANY
    listen ls 10
    forever $ accept ls >>= (forkIO . go)
    where
    go (s, sockaddr@(SockAddrInet _ _)) =
        let
            pName = BC.pack $ show sockaddr
            handlers = [
                Handler (\(e :: IOException) -> handleEx e),
                Handler (\(e :: ErrorCall) -> handleEx e),
                Handler (\(e :: AsyncException) -> handleAsync e)
                ]
            handleEx e = atomically $ maybeLog sess Medium $ B.concat
                ["Caught exception in peer handler. ", pName, ": ",
                 BC.pack $ show e]
            handleAsync e = if e == ThreadKilled then handleEx e else throwIO e
            in flip catches handlers $ do
        atomically . maybeLog sess Debug $ BC.concat [
            "New incoming connection: ", pName]
        interested' <- newTVarIO False
        theirHandshake <- getHandshake s
        let
            peerSt = PeerSt
                {connectedPeerSt = Just cPeerSt, pSockAddr = sockaddr}
            cPeerSt = ConnectedPeerSt {
                peerId = hPeerId theirHandshake,
                interested = interested' }
        maybeLogPeer sess peerSt Debug $ B.concat
            ["Got handshake: ", (BC.pack $ show theirHandshake)]
        tid <- myThreadId
        mbtorst <- atomically $ do
            mbtorst' <- M.lookup (hInfoHash theirHandshake) <$>
                (readTVar $ torrents sess)
            case mbtorst' of
                Nothing -> return mbtorst'
                Just torst -> do
                    torIsRunning <- (==Running) <$> readTVar (sActivity torst)
                    peercount <- M.size <$> readTVar (sPeers torst)
                    wereAlreadyConnected <-
                        F.any (sameIp sockaddr . pSockAddr) <$>
                            (readTVar . sPeers $ torst)
                    if torIsRunning &&
                       peercount <= 50 &&
                       (not wereAlreadyConnected)
                        then do
                            modifyTVar (sPeers torst) (M.insert tid peerSt)
                            return mbtorst'
                        else return Nothing
        case mbtorst of
            Nothing -> error "Refusing connection."
            Just torst -> flip finally
                (atomically $ modifyTVar (sPeers torst) (M.delete tid)) $ do
                    maybeLogPeer sess peerSt Low "Connected."
                    sendHandshake sess torst s
                    sendFullBitfield torst s
                    sendPeerMsg s Unchoke
                    peerHandler sess torst peerSt s
    go (_, _) =
        error "Got non-IPv4 SockAddr in peerListener"
    sameIp (SockAddrInet _ host1) (SockAddrInet _ host2) = host1 == host2
    sameIp _                      _                      =
        error "Got non-IPv4 SockAddr in peerListener"

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar tv f = readTVar tv >>= (writeTVar tv . f)

peerHandler :: Session -> TorrentSt -> PeerSt -> Socket -> IO ()
peerHandler sess torst peerSt s = bracket
    -- This is a TVar Set rather than a TChan because we need to
    -- support removing requests from the queue. Note this
    -- implementation doesn't preserve request ordering, but the other
    -- clients don't seem to care and the spec isn't explicit.
    (do
        pieceReqs <- newTVarIO S.empty
        tid <- forkIO $ peerWriter torst pieceReqs s
        return (pieceReqs, tid))
    (killThread . snd)
    (peerReader . fst)
    where
    peerReader pieceReqs = do
        iter <- enumSocket s $ joinI $ enumPeerMsg $ foreachI $
            handler sess peerSt pieceReqs
        run iter

handler ::
    Session -> PeerSt -> TVar (S.Set (PieceNum, Word32, Word32)) -> PeerMsg ->
    IO Bool
handler sess peerSt pieceReqs it = do
    maybeLogPeer sess peerSt Debug $ B.concat
        ["Got message: ", BC.pack $ show it]
    case it of
        Request pn off len -> atomically $
            modifyTVar pieceReqs $ S.insert (pn, off, len)
        Cancel pn off len -> atomically $
            modifyTVar pieceReqs $ S.delete (pn, off, len)
        Interested -> atomically $
            writeTVar (interested . fromConnectedPeerSt $ peerSt) True
        NotInterested -> atomically $
            writeTVar (interested . fromConnectedPeerSt $ peerSt) False
        _ -> return ()
    return True

peerWriter ::
    TorrentSt -> TVar (S.Set (PieceNum, Word32, Word32)) -> Socket -> IO ()
peerWriter torst pieceReqs s = loop
--FIXME Need an exception handler here so the reader thread doesn't get lonely.
    where
    loop = do
        (pn, offset, len) <- atomically $ do
            pieceReqs' <- readTVar pieceReqs
            case S.minView pieceReqs' of
                Nothing -> retry
                Just (req, pieceReqs'') -> do
                    writeTVar pieceReqs pieceReqs''
                    return req
        dataToSend <-
            (B.take (fromIntegral len) . B.drop (fromIntegral offset)) <$>
            fromJust <$> getPiece torst pn
        sendPeerMsg s $ Piece pn offset dataToSend
        loop

-- Poor man's mapStreamM?
foreachI :: Monad m => (el -> m Bool) -> IterateeG [] el m ()
foreachI f = IterateeG step
    where
    step s@(EOF Nothing)    = return $ Done () s
    step   (EOF (Just err)) = return $ Cont (throwErr err) (Just err)
    step   (Chunk [])       = return $ Cont (foreachI f) Nothing
    step   (Chunk (x:xs))       = do
        continue <- f x
        if continue then step (Chunk xs) else return $ Done () (Chunk xs)

-- Enumerator of BT PeerMsgs with length prefixes.
enumPeerMsg :: (Monad m, Functor m) =>
    EnumeratorN WrappedByteString Word8 [] PeerMsg m a
enumPeerMsg = convStream convPeerMsgs
    where
    convPeerMsgs = eitherToMaybe <$> checkErr ((:[]) <$> getPeerMsg)
    eitherToMaybe (Left  _) = Nothing
    eitherToMaybe (Right x) = Just x

getPeerMsg :: Monad m => IterateeG WrappedByteString Word8 m PeerMsg
getPeerMsg = do
    len <- endianRead4 MSB
    case len of
        0 -> getPeerMsg -- Zero length messages are keepalives.
        _ -> joinI $ Iter.take (fromIntegral len) decodeI

-- | Run an iteratee over input from a socket. The socket must be connected.
-- This is equivalent to enumFd modulo the blocking problem. Totally did not
-- realize you could call read() on a socket. Once the blocking issue with
-- enumFd is fixed, this should be deleted.
enumSocket :: Socket -> EnumeratorGM WrappedByteString Word8 IO a
enumSocket s iter = do
    bs <- SB.recv s 4096
    case B.length bs of
        0 -> return iter
        _ -> do
            igv <- runIter iter (Chunk $ WrapBS bs)
            case igv of
                Done x _        -> return . return $ x
                Cont i Nothing  -> enumSocket s i
                Cont _ (Just e) -> return $ throwErr e

sendHandshake :: Session -> TorrentSt -> Socket -> IO ()
sendHandshake sess torst s = SBL.sendAll s $ encode Handshake {
    hResByte0 = 0, hResByte1 = 0, hResByte2 = 0, hResByte3 = 0, hResByte4 = 0,
    hResByte5 = 0, hResByte6 = 0, hResByte7 = 0,
    hInfoHash = tInfohash $ sTorrent torst,
    hPeerId = sPeerId sess}

-- | Get the peer's handshake, making sure any exceptions from binary are
-- thrown here and not in another thread or outside the scope of appropriate
-- handlers.
getHandshake :: Socket -> IO Handshake
getHandshake s = (decode <$> recvAll s 68) >>= evaluate

-- | Send an all \255 bitfield of appropriate length.
sendFullBitfield :: TorrentSt -> Socket -> IO ()
sendFullBitfield torst s = let
    numPieces = snd $ bounds $ tPieceHashes $ sTorrent torst
    (quot', rem') = quotRem numPieces 8
    bitFieldLen =
        fromIntegral $ if rem' /= 0 then quot'+1 else quot' in
    sendPeerMsg s . Bitfield $ B.replicate bitFieldLen 255

sendPeerMsg :: Socket -> PeerMsg -> IO ()
sendPeerMsg s p = SBL.sendAll s $ runPut $ do
    let p' = encode p
    put (fromIntegral $ L.length p' :: Word32)
    putLazyByteString p'

-- Get the specified number of bytes from a socket, retrying until done.
recvAll :: Socket -> Int64 -> IO L.ByteString
recvAll s numBytes = do
    whatWeGot <- SBL.recv s numBytes
    let count = L.length whatWeGot
    case False of
        _ | count == 0 -> error "recvAll: couldn't get sufficient bytes"
          | count < numBytes -> do
                rest <- recvAll s (numBytes - fromIntegral count)
                return $ L.append whatWeGot rest
          | count == numBytes -> return whatWeGot
          | otherwise -> error "recvAll: the impossible happened"

maybeLogPeer :: Session -> PeerSt -> LogLevel -> B.ByteString -> IO ()
maybeLogPeer sess peerSt lvl msg =
    atomically $ maybeLog sess lvl $ B.concat
        ["(", BC.pack . show $ pSockAddr peerSt, ") ", msg]
