module Internal.Peers.Handler
    (
    PeerSt(..),
    ConnectedPeerSt(..),
    connectToPeer,
    peerListener
    ) where

import Prelude hiding (catch)

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
import Data.Iteratee.WrappedByteString
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
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
    they'reInterested :: TVar Bool,
    we'reInterested :: TVar Bool,
    they'reChoked :: TVar Bool,
    we'reChoked :: TVar Bool,
    chokeTimer :: TVar (TVar Bool) -- Used with genericRegisterDelay

    -- Later we'll have a TChan of the have messages to send, dupTChan'd from
    -- the global one and a bitfield.
    }

mkConnectedPeerSt :: B.ByteString -> IO ConnectedPeerSt
mkConnectedPeerSt peerId' = ConnectedPeerSt peerId'
    <$> newTVarIO False
    <*> newTVarIO False
    <*> newTVarIO True
    <*> newTVarIO True
    <*> (newTVarIO False >>= newTVarIO)

-- | Connect to a new peer.
connectToPeer ::
    Session -> TorrentSt -> SockAddr -> IO ()
connectToPeer sess torst sockAddr =
    (block . forkIO $ logEx sess (show sockAddr ++ "(read/init)") go) >> return ()
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
                        F.any (sameIp sockAddr . pSockAddr) <$>
                            (readTVar . sPeers $ torst)
                    when wereAlreadyConnected $ error
                        "Already connected to this peer in a different thread."
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
                maybeLogPeer sess peerSt Debug $ concat
                    ["Got handshake: ", show theirHandshake]
                when (hInfoHash theirHandshake /= tInfohash (sTorrent torst)) $
                    error "Wrong infohash in outgoing peer connection!"
                cPeerSt <- mkConnectedPeerSt $ hPeerId theirHandshake
                let
                    peerSt' = PeerSt {
                        connectedPeerSt = Just cPeerSt,
                        pSockAddr = pSockAddr peerSt}
                tid <- myThreadId
                atomically $ modifyTVar (sPeers torst) (M.insert tid peerSt')
                -- Overwrites the unconnected PeerSt inserted by getNextPeer.
                peerHandler sess torst peerSt' s
                maybeLogPeer sess peerSt' Low "Disconnecting (normal).")

peerListener :: Session -> PortNumber -> IO ()
peerListener sess p = bracket (socket AF_INET Stream 0) sClose $ \ls -> do
    bindSocket ls $ SockAddrInet p iNADDR_ANY
    listen ls 10
    forever $ accept ls >>= (forkIO . go)
    where
    go (s, sockaddr@(SockAddrInet _ _)) =
        logEx sess (show sockaddr ++ "(read/init)") $ do
            atomically . maybeLog sess Low $
                concat ["New incoming connection: ", show sockaddr]
            theirHandshake <- getHandshake s
            cPeerSt <- mkConnectedPeerSt $ hPeerId theirHandshake
            let
                peerSt = PeerSt
                    {connectedPeerSt = Just cPeerSt, pSockAddr = sockaddr}
            maybeLogPeer sess peerSt Debug $ concat
                ["Got handshake: ", show theirHandshake]
            tid <- myThreadId
            mbtorst <- atomically $ do
                mbtorst' <- M.lookup (hInfoHash theirHandshake) <$>
                    readTVar (torrents sess)
                case mbtorst' of
                    Nothing -> return mbtorst'
                    Just torst -> do
                        torIsRunning <- (==Running) <$>
                            readTVar (sActivity torst)
                        peercount <- M.size <$> readTVar (sPeers torst)
                        wereAlreadyConnected <-
                            F.any (sameIp sockaddr . pSockAddr) <$>
                                (readTVar . sPeers $ torst)
                        if torIsRunning &&
                           peercount <= 50 &&
                           not wereAlreadyConnected
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
                        peerHandler sess torst peerSt s
    go (_, _) =
        error "Got non-IPv4 SockAddr in peerListener"

sameIp :: SockAddr -> SockAddr -> Bool
sameIp (SockAddrInet _ host1) (SockAddrInet _ host2) = host1 == host2
sameIp _                      _                      =
    error "Got non-IPv4 SockAddr in sameIp"

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar tv f = readTVar tv >>= (writeTVar tv . f)

peerHandler :: Session -> TorrentSt -> PeerSt -> Socket -> IO ()
peerHandler sess torst peerSt s = do
    sendFullBitfield torst s
    bracket
    -- pieceReqs is a TVar Set rather than a TChan because we need to
    -- support removing requests from the queue. Note this
    -- implementation doesn't preserve request ordering, but the other
    -- clients don't seem to care and the spec isn't explicit.
        (do
            readerTid <- myThreadId
            pieceReqs <- newTVarIO S.empty
            writerTid <- forkIO $
                -- Make sure the reader doesn't get widowed if the writer throws
                -- an exception and that exceptions are logged.
                logEx sess (show (pSockAddr peerSt) ++ "(writer)") $ onException
                    (peerWriter sess torst peerSt pieceReqs s)
                    (killThread readerTid)
            return (pieceReqs, writerTid))
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
    maybeLogPeer sess peerSt Debug $ "Got message: " ++ show it
    case it of
        Request pn off len -> atomically $ do
            choked <- readTVar . they'reChoked . fromConnectedPeerSt $ peerSt
            unless choked $ modifyTVar pieceReqs $ S.insert (pn, off, len)
        Cancel pn off len -> atomically $
            modifyTVar pieceReqs $ S.delete (pn, off, len)
        Interested -> atomically $ do
            writeTVar (they'reInterested . fromConnectedPeerSt $ peerSt) True
        NotInterested -> atomically $
            writeTVar (they'reInterested . fromConnectedPeerSt $ peerSt) False
        _ -> return ()
    return True

peerWriter ::
    Session -> TorrentSt -> PeerSt -> TVar (S.Set (PieceNum, Word32, Word32)) ->
    Socket -> IO ()
peerWriter sess torst peerSt pieceReqs s = do
    -- Track the interest/choke state as visible to the peer.
    we'reInterested' <- newTVarIO False
    they'reChoked' <- newTVarIO True
    forever . join . atomically $ orElse
        (updateChokeOrInterest
            sess peerSt we'reInterested' they'reChoked' pieceReqs s)
        (sendPiece torst pieceReqs s)

updateChokeOrInterest ::
    Session -> PeerSt -> TVar Bool -> TVar Bool ->
    TVar (S.Set (PieceNum, Word32, Word32)) -> Socket -> STM (IO ())
updateChokeOrInterest sess peerSt we'reInterested' they'reChoked' pieceReqs s = do
    oldInterest <- readTVar we'reInterested'
    newInterest <- readTVar . we'reInterested . fromConnectedPeerSt $ peerSt
    oldChoking <- readTVar they'reChoked'
    newChoking <- readTVar . they'reChoked . fromConnectedPeerSt $ peerSt
    let
        updateInterest = oldInterest /= newInterest
        updateChoking = oldChoking /= newChoking
        mbInterestMsg =
            updateMessage Interested NotInterested updateInterest oldInterest
        mbChokingMsg =
            updateMessage Choke Unchoke updateChoking oldChoking
    unless (updateInterest || updateChoking) retry
    writeTVar we'reInterested' newInterest
    writeTVar they'reChoked' newChoking
    when newChoking $ writeTVar pieceReqs S.empty
    return . go . catMaybes $ [mbInterestMsg, mbChokingMsg]
    where
    go msgsToSend = do
        maybeLogPeer sess peerSt Debug $
            "Sending message(s): " ++ show msgsToSend
        mapM_ (sendPeerMsg s) msgsToSend
    updateMessage trueMsg falseMsg update prevState = if update
        then Just $ if prevState then falseMsg else trueMsg
        else Nothing

sendPiece ::
    TorrentSt -> TVar (S.Set (PieceNum, Word32, Word32)) -> Socket ->
    STM (IO ())
sendPiece torst pieceReqs s = do
    pieceReqs' <- readTVar pieceReqs
    case S.minView pieceReqs' of
        Nothing -> retry
        Just (req, pieceReqs'') -> do
            writeTVar pieceReqs pieceReqs''
            return $ go req
    where
    go (pn, offset, len) = do
        dataToSend <-
            (B.take (fromIntegral len) . B.drop (fromIntegral offset)) <$>
            fromJust <$> getPiece torst pn
        sendPeerMsg s $ Piece pn offset dataToSend

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

maybeLogPeer :: Session -> PeerSt -> LogLevel -> String -> IO ()
maybeLogPeer sess peerSt lvl msg =
    atomically $ maybeLog sess lvl $ concat
        ["(", show $ pSockAddr peerSt, ") ", msg]

-- Used near the beginning of a thread to record exceptions in session-global
-- log before the thread dies.
logEx :: Session -> String -> IO () -> IO ()
logEx sess str action = catch action $ \(e :: SomeException) -> do
    atomically . maybeLog sess Low $
        "Caught exception in " ++ str ++ ": " ++ show e ++ "thread dying."
