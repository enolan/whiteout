-- This module exists so we can quickcheck the binary instances without exposing
-- the types in Internal.Peer
module Internal.Peer.Messages
    (
    Handshake(..),
    PeerMsg(..),
    enumSocket
    )
    where

import Control.Applicative
import Control.Monad (liftM3, replicateM)
import Data.Binary
import Data.Binary.Get (getByteString, remaining)
import Data.Binary.Put (putByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 () -- isString instance
import Data.Iteratee.Base as Iter
import Data.Iteratee.WrappedByteString
import Data.Word (Word8, Word32)
import Network.Socket
import qualified Network.Socket.ByteString as SB

import Internal.Types

data Handshake = Handshake {
    resByte0 :: Word8,
    resByte1 :: Word8,
    resByte2 :: Word8,
    resByte3 :: Word8,
    resByte4 :: Word8,
    resByte5 :: Word8,
    resByte6 :: Word8,
    resByte7 :: Word8,
    hInfoHash :: B.ByteString,
    hPeerId :: B.ByteString
    } deriving (Show, Eq)

instance Binary Handshake where
    put h = do
        put (19 :: Word8)
        putByteString "BitTorrent protocol"
        mapM_ (put . ($h))
            [resByte0, resByte1, resByte2, resByte3, resByte4, resByte5,
             resByte6, resByte7]
        putByteString $ hInfoHash h
        putByteString $ hPeerId h
    get = do
        (l :: Word8) <- get
        if l == 19 then return () else error "bad handshake (length byte)"
        str <- getByteString 19
        if str == "BitTorrent protocol"
            then return ()
            else error "bad handshake (protocol name)"
        [resByte0, resByte1, resByte2, resByte3, resByte4, resByte5, resByte6,
         resByte7] <- replicateM 8 get
        infoHash <- getByteString 20
        peerId <- getByteString 20
        return Handshake {
            resByte0 = resByte0, resByte1 = resByte1, resByte2 = resByte2,
            resByte3 = resByte3, resByte4 = resByte4, resByte5 = resByte5,
            resByte6 = resByte6, resByte7 = resByte7,
            hInfoHash = infoHash,
            hPeerId = peerId
            }

data PeerMsg =
    Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have PieceNum
  | Bitfield B.ByteString
  | Request PieceNum Word32 Word32
  | Piece PieceNum Word32 B.ByteString
  | Cancel PieceNum Word32 Word32
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

-- If I want to get this into the iteratee library it probably needs to use the
-- ReadableChunk class rather than Network.Socket.ByteString. But I really don't
-- feel like it today and Whiteout doesn't need or want to use anything but
-- ByteStrings.
-- | Run an iteratee over input from a socket. The socket must be connected.
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
