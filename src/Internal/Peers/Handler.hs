module Internal.Peers.Handler
    (
    PeerSt(..),
    connectToPeer
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Array.IArray (bounds)
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Iteratee.Base as Iter
import Data.Iteratee.Binary
import Data.Iteratee.WrappedByteString
import Data.Maybe (fromJust)
import qualified Data.Map as M
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
    pieceReqs :: TVar (S.Set (PieceNum, Word32, Word32)),
    -- ^ Pieces in the pipeline, to be sent.
    peerId :: B.ByteString,
    interested :: TVar Bool

    -- Later we'll have a TChan of the have messages to send, dupTChan'd from
    -- the global one, and a bitfield, and track choke/interest state here.
    }

-- | Connect to a new peer.
connectToPeer ::
    Session -> TorrentSt -> PeerSt -> IO ThreadId
connectToPeer sess torst peerSt = forkIO $ catches go handlers
    where
        -- We need to ensure that we either have our ThreadId in
        -- sConnectionsInProgress or have our PeerSt in sPeers. Otherwise, we
        -- could be left running when the peer manager kills all the peer
        -- threads to stop the torrent.
        go = bracket
            (socket AF_INET Stream 0)
            (\s -> do
                sClose s
                tid <- myThreadId
                atomically $ modifyTVar (sPeers torst) (M.delete tid))
            (\s -> do
                maybeLogPeer sess peerSt Low "Connecting"
                connect s $ pSockAddr peerSt
                sendHandshake sess torst s
                theirHandshake :: Handshake <- decode <$> recvAll s 68
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
                reqQueue <- newTVarIO S.empty
                interested' <- newTVarIO False
                let
                    cPeerSt = ConnectedPeerSt {
                        pieceReqs = reqQueue,
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
             BC.pack . show . pSockAddr $ peerSt,
             ": ", BC.pack $ show e]

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar tv f = readTVar tv >>= (writeTVar tv . f)

peerHandler :: Session -> TorrentSt -> PeerSt -> Socket -> IO ()
peerHandler sess torst peerSt s = bracket
        (forkIO $ peerWriter torst (fromConnectedPeerSt peerSt) s)
        killThread
        (const peerReader)
    where
        peerReader = do
            iter <- enumSocket s $ joinI $ enumPeerMsg $ foreachI $
                handler sess peerSt
            run iter

handler :: Session -> PeerSt -> PeerMsg -> IO Bool
handler sess peerSt it = do
    maybeLogPeer sess peerSt Debug $ B.concat
        ["Got message: ", BC.pack $ show it]
    case it of
        Request pn off len -> atomically $
            modifyTVar (pieceReqs . fromConnectedPeerSt $ peerSt) $
                S.insert (pn, off, len)
        Cancel pn off len -> atomically $
            modifyTVar (pieceReqs . fromConnectedPeerSt $ peerSt) $
                S.delete (pn, off, len)
        Interested -> atomically $
            writeTVar (interested . fromConnectedPeerSt $ peerSt) True
        NotInterested -> atomically $
            writeTVar (interested . fromConnectedPeerSt $ peerSt) False
        _ -> return ()
    return True

peerWriter :: TorrentSt -> ConnectedPeerSt -> Socket -> IO ()
peerWriter torst (ConnectedPeerSt {pieceReqs = pieceReqs'}) s = loop
    where
    loop = do
        (pn, offset, len) <- atomically $ do
            pieceReqs'' <- readTVar pieceReqs'
            if S.null pieceReqs''
                then retry
                else do
                    let (req, pieceReqs''') = S.deleteFindMin pieceReqs''
                    writeTVar pieceReqs' pieceReqs'''
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
