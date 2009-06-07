module Internal.Peer
    (
    addPeer
    ) where

import Control.Applicative
import Control.Concurrent (forkIO)
import Data.Array.IArray (bounds)
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromJust)
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
addPeer sess torst h p = (forkIO go) >> return ()
    where
        go = do
            s <- socket AF_INET Stream 0
            addr <- inet_addr h
            connect s $ SockAddrInet p addr
            sendHandshake sess torst s
            theirHandshake :: Handshake <- decode <$> SBL.recv s 68
            print theirHandshake
            let
                numPieces = snd $ bounds $ pieceHashes $ torrent $ torst
                (quot, rem) = quotRem numPieces 8
                bitFieldLen = fromIntegral $ if rem /= 0 then quot+1 else quot
            sendPeerMsg s $ Bitfield $ B.replicate bitFieldLen 255
            sendPeerMsg s $ Unchoke
            peerHandler torst s

-- PeerMsg handling loop
peerHandler :: TorrentSt -> Socket -> IO ()
peerHandler torst s = do
    msgLen :: Word32 <- decode <$> SBL.recv s 4
    if msgLen > 0
        then do msg <- decode <$> SBL.recv s (fromIntegral msgLen)
                case msg of
                    it@(Request pn offset len) -> do
                        print it
                        dataToSend <-
                            (B.take (fromIntegral len) .
                                B.drop (fromIntegral offset)) <$>
                            fromJust <$> getPiece torst pn
                        let msgToSend = Piece pn offset dataToSend
                        sendPeerMsg s msgToSend >> peerHandler torst s
                    other -> print other >> peerHandler torst s
        else peerHandler torst s

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
