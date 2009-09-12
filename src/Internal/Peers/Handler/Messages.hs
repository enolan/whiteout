-- This module exists so we can test the message (de)serialization without
-- exposing it from Internal.Peers.Handler
module Internal.Peers.Handler.Messages
    (
    Handshake(..),
    PeerMsg(..),
    decodeI
    )
    where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Get (getByteString, remaining)
import Data.Binary.Put (putByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 () -- isString instance
import Data.Iteratee.Base
import Data.Iteratee.WrappedByteString
import Data.Word (Word8, Word32)

import Internal.Types

data Handshake = Handshake {
    hResByte0 :: !Word8,
    hResByte1 :: !Word8,
    hResByte2 :: !Word8,
    hResByte3 :: !Word8,
    hResByte4 :: !Word8,
    hResByte5 :: !Word8,
    hResByte6 :: !Word8,
    hResByte7 :: !Word8,
    hInfoHash :: !B.ByteString,
    hPeerId :: !B.ByteString
    } deriving (Show, Eq)

instance Binary Handshake where
    put h = do
        put (19 :: Word8)
        putByteString "BitTorrent protocol"
        mapM_ (put . ($h))
            [hResByte0, hResByte1, hResByte2, hResByte3, hResByte4, hResByte5,
             hResByte6, hResByte7]
        putByteString $ hInfoHash h
        putByteString $ hPeerId h
    get = do
        (l :: Word8) <- get
        unless (l == 19) $ error "bad handshake (length byte)"
        str <- getByteString 19
        unless (str == "BitTorrent protocol") $
            error "bad handshake (protocol name)"
        [resByte0, resByte1, resByte2, resByte3, resByte4, resByte5, resByte6,
         resByte7] <- replicateM 8 get
        infoHash <- getByteString 20
        peerId <- getByteString 20
        return Handshake {
            hResByte0 = resByte0, hResByte1 = resByte1, hResByte2 = resByte2,
            hResByte3 = resByte3, hResByte4 = resByte4, hResByte5 = resByte5,
            hResByte6 = resByte6, hResByte7 = resByte7,
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

-- Accumulate chunks of stream until eof, then run decode on the accumulated
-- bytestring.
decodeI :: (Binary a, Monad m) => IterateeG WrappedByteString Word8 m a
decodeI = IterateeG $ step []
    where
    step :: (Binary a, Monad m) =>
        [B.ByteString] -> StreamG WrappedByteString Word8 ->
        m (IterGV WrappedByteString Word8 m a)
    step xs sg@(EOF Nothing)    =
        return $ Done (decode $ L.fromChunks $ reverse xs) sg
    step _xs (EOF (Just err))   = return $ Cont (throwErr err) (Just err)
    step xs (Chunk (WrapBS bs)) =
        return $ Cont (IterateeG $ step (bs:xs)) Nothing
