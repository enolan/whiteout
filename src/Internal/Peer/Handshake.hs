{-# OPTIONS_GHC -fno-warn-orphans #-}
-- This module exists so we can quickcheck the binary instance without exposing
-- Handshake from Internal.Peer.
module Internal.Peer.Handshake (Handshake(..)) where

import Data.Binary
import Data.Binary.Get (getBytes)
import Data.Binary.Put (putByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 () -- isString instance
import Data.Word (Word8)

data Handshake = Handshake {
    resByte0 :: Word8,
    resByte1 :: Word8,
    resByte2 :: Word8,
    resByte3 :: Word8,
    resByte4 :: Word8,
    resByte5 :: Word8,
    resByte6 :: Word8,
    resByte7 :: Word8,
    infoHash :: B.ByteString,
    peerId :: B.ByteString
    } deriving (Show, Eq)

instance Binary Handshake where
    put h = do
        put (19 :: Word8)
        putByteString "BitTorrent protocol"
        sequence_ $ map put $ map ($h)
            [resByte0, resByte1, resByte2, resByte3, resByte4, resByte5,
             resByte6, resByte7]
        putByteString $ infoHash h
        putByteString $ peerId h
    get = do
        (l :: Word8) <- get
        if l == 19 then return () else error "bad handshake (length byte)"
        str <- getBytes 19
        if str == "BitTorrent protocol"
            then return ()
            else error "bad handshake (protocol name)"
        [resByte0, resByte1, resByte2, resByte3, resByte4, resByte5, resByte6,
         resByte7] <- sequence $ replicate 8 get
        infoHash <- getBytes 20
        peerId <- getBytes 20
        return $ Handshake {
            resByte0 = resByte0, resByte1 = resByte1, resByte2 = resByte2,
            resByte3 = resByte3, resByte4 = resByte4, resByte5 = resByte5,
            resByte6 = resByte6, resByte7 = resByte7,
            infoHash = infoHash,
            peerId = peerId
            }

