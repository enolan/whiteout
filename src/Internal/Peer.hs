module Internal.Peer
    (
    PeerMsg(..),
    addPeer
    ) where

import Data.Binary
import Data.Binary.Get (getByteString, remaining)
import Data.Binary.Put (putByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int32)
import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Monad (liftM3)
import Network.Socket
import qualified Network.Socket.ByteString as SB
import qualified Network.Socket.ByteString.Lazy as SBL

import Internal.Peer.Handshake
import Internal.Types

data PeerMsg =
    Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have PieceNum
  | Bitfield ByteString
  | Request PieceNum Int32 Int32
  | Piece PieceNum Int32 ByteString
  | Cancel PieceNum Int32 Int32
    deriving (Show, Eq)

instance Binary PeerMsg where
    put Choke               = put (0 :: Word8)
    put Unchoke             = put (1 :: Word8)
    put Interested          = put (2 :: Word8)
    put NotInterested       = put (3 :: Word8)
    put (Have n)            = put (4 :: Word8) >> put n
    put (Bitfield b)        = put (5 :: Word8) >> putByteString b
    put (Request n off len) = put (6 :: Word8) >> put n >> put off >> put len
    put (Piece n off bs)    = do
        put (7 :: Word8)
        put n
        put off
        putByteString bs
    put (Cancel n off len)  = put (8 :: Word8) >> put n >> put off >> put len

    get = do
        (t :: Word8) <- get
        case t of
            0 -> return Choke
            1 -> return Unchoke
            2 -> return Interested
            3 -> return NotInterested
            4 -> Have <$> get
            5 -> Bitfield <$> ((fromIntegral <$> remaining) >>= getByteString)
            6 -> liftM3 Request get get get
            7 -> do
                n <- get
                off <- get
                bs <- (fromIntegral <$> remaining) >>= getByteString
                return $ Piece n off bs
            8 -> liftM3 Cancel get get get
            _ -> error "Invalid interpeer message, couldn't read tag."

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
            peerHandler torst s


peerHandler :: TorrentSt -> Socket -> IO ()
peerHandler torst s = do
    return ()

sendHandshake :: Session -> TorrentSt -> Socket -> IO ()
sendHandshake sess torst s = SBL.sendAll s $ encode Handshake {
    resByte0 = 0, resByte1 = 0, resByte2 = 0, resByte3 = 0, resByte4 = 0,
    resByte5 = 0, resByte6 = 0, resByte7 = 0,
    infoHash = infohash $ torrent torst,
    Internal.Peer.Handshake.peerId = Internal.Types.peerId sess} -- Ugh.

