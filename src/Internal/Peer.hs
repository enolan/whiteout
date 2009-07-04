module Internal.Peer
    (
    addPeer
    ) where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception
import Data.Array.IArray (bounds)
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Iteratee.Base as Iter
import Data.Iteratee.WrappedByteString
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Network.Socket
import qualified Network.Socket.ByteString as SB
import qualified Network.Socket.ByteString.Lazy as SBL

import Internal.Peer.Messages
import Internal.Pieces
import Internal.Types

-- | Connect to a new peer. Later this should add them to a queue and another
-- thread should empty the queue with a limit on the max open/half-open
-- connections.
addPeer :: Session -> TorrentSt -> String -> PortNumber -> IO ()
addPeer sess torst h p = (forkIO $ catches go handlers) >> return ()
    where
        go = bracket (socket AF_INET Stream 0) sClose $ \s-> do
            addr <- inet_addr h
            connect s $ SockAddrInet p addr
            sendHandshake sess torst s
            theirHandshake :: Handshake <- decode <$> recvAll s 68
            print theirHandshake
            let
                numPieces = snd $ bounds $ pieceHashes $ torrent torst
                (quot, rem) = quotRem numPieces 8
                bitFieldLen = fromIntegral $ if rem /= 0 then quot+1 else quot
            sendPeerMsg s $ Bitfield $ B.replicate bitFieldLen 255
            sendPeerMsg s Unchoke
            peerHandler torst s
        handlers = [
            Handler (\(e :: IOException) -> print e),
            Handler (\(e :: ErrorCall) -> print e)
            ]

peerHandler :: TorrentSt -> Socket -> IO ()
peerHandler torst s = do
    peerSt <- atomically newPeerSt
    forkIO $ peerWriter torst peerSt s
    (enumSocket s $ joinI $ enumPeerMsg (foreachI $ handler peerSt)) >>= run

handler :: PeerSt -> PeerMsg -> IO ()
handler peerSt it@(Request pn offset len)   = do
    print it
    atomically $ do
        pieceReqs' <- readTVar $ pieceReqs peerSt
        writeTVar (pieceReqs peerSt) $
            S.insert (pn, offset, len) pieceReqs'
handler _peerSt it                          = print it

-- | The state associated with a peer connection. Used for communication
-- between the reader thread, the writer thread and, when it's actually written,
-- the peer manager.
data PeerSt = PeerSt {
    pieceReqs :: TVar (S.Set (PieceNum, Word32, Word32))
    -- ^ Pieces in the pipeline, to be sent.

    -- Later we'll have a TChan of the have messages to send, dupTChan'd from
    -- the global one, and a bitfield, and track choke/interest state here.
    }

newPeerSt :: STM PeerSt
newPeerSt = PeerSt <$> newTVar S.empty

peerWriter :: TorrentSt -> PeerSt -> Socket -> IO ()
peerWriter torst (PeerSt {pieceReqs = pieceReqs}) s = loop
    where
    loop = do
        (pn, offset, len) <- atomically $ do
            pieceReqs' <- readTVar pieceReqs
            case S.null pieceReqs' of
                True -> retry
                False -> do
                    let (req, pieceReqs'') = S.deleteFindMin pieceReqs'
                    writeTVar pieceReqs pieceReqs''
                    return req
        dataToSend <-
            (B.take (fromIntegral len) . B.drop (fromIntegral offset)) <$>
            fromJust <$> getPiece torst pn
        sendPeerMsg s $ Piece pn offset dataToSend
        loop

-- Poor man's mapStreamM?
foreachI :: Monad m => (el -> m ()) -> IterateeG [] el m ()
foreachI f = IterateeG step
    where
    step s@(EOF Nothing)    = return $ Done () s
    step   (EOF (Just err)) = return $ Cont (throwErr err) (Just err)
    step   (Chunk [])       = return $ Cont (foreachI f) Nothing
    step   (Chunk xs)       = do
        mapM_ f xs
        return $ Cont (foreachI f) Nothing

-- Enumerator of BT PeerMsgs with length prefixes.
enumPeerMsg :: (Monad m, Functor m) =>
    EnumeratorN WrappedByteString Word8 [] PeerMsg m a
enumPeerMsg = convStream convPeerMsgs
    where
    convPeerMsgs = eitherToMaybe <$> checkErr ((\x -> [x]) <$> getPeerMsg)
    eitherToMaybe (Left  _) = Nothing
    eitherToMaybe (Right x) = Just x

getPeerMsg :: Monad m => IterateeG WrappedByteString Word8 m PeerMsg
getPeerMsg = do
    len :: Word32 <- joinI $ Iter.take 4 decodeI
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
        0 -> enumErr "Remote closed socket" iter
        _ -> do
            igv <- runIter iter (Chunk $ WrapBS bs)
            case igv of
                Done x _        -> return . return $ x
                Cont i Nothing  -> enumSocket s i
                Cont _ (Just e) -> return $ throwErr e

sendHandshake :: Session -> TorrentSt -> Socket -> IO ()
sendHandshake sess torst s = SBL.sendAll s $ encode Handshake {
    resByte0 = 0, resByte1 = 0, resByte2 = 0, resByte3 = 0, resByte4 = 0,
    resByte5 = 0, resByte6 = 0, resByte7 = 0,
    hInfoHash = tInfohash $ torrent torst,
    hPeerId = sPeerId sess}

sendPeerMsg :: Socket -> PeerMsg -> IO ()
sendPeerMsg s p = SBL.sendAll s $ runPut $ do
    let p' = encode p
    put ((fromIntegral $ L.length p') :: Word32)
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
