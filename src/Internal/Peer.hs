module Internal.Peer where

import Data.Binary
import Data.Binary.Get (getByteString, remaining)
import Data.Binary.Put (putByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int32)
import Control.Applicative
import Control.Monad (liftM3)

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
