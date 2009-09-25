{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Internal.Peers.Handler.Messages (theTests) where

import Control.Applicative
import Control.Monad (ap, replicateM)
import Control.Monad.Identity
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Iteratee.Base
import Data.Iteratee.WrappedByteString
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Test.ArbitraryInstances ()
import Internal.Peers.Handler.Messages

theTests :: Test
theTests = testGroup "Internal.Peer.Messages" [
    testProperty "handshakeRoundTrip" handshakeRoundTrip,
    testProperty "peerMsgRoundTrip" peerMsgRoundTrip,
    testProperty "decodeI1ChunkRoundTrip - PeerMsg"
        (decodeI1ChunkRoundTrip :: PeerMsg -> Bool),
    testProperty "decodeI1ChunkRoundTrip - Handshake"
        (decodeI1ChunkRoundTrip :: Handshake -> Bool),
    testProperty "decodeI1ChunkRoundTrip - Word32"
        (decodeI1ChunkRoundTrip :: Word32 -> Bool),
    testProperty "decodeINChunkRoundTrip - PeerMsg"
        (decodeINChunkRoundTrip :: PeerMsg -> Positive Int -> Bool),
    testProperty "decodeINChunkRoundTrip - Handshake"
        (decodeINChunkRoundTrip :: Handshake -> Positive Int -> Bool),
    testProperty "decodeINChunkRoundTrip - Word32"
        (decodeINChunkRoundTrip :: Word32 -> Positive Int -> Bool),
    testProperty "enumPeerMsgRoundTrip1Chunk" enumPeerMsgRoundTrip1Chunk,
    testProperty "enumPeerMsgRoundTripNChunk" enumPeerMsgRoundTripNChunk
    ]

handshakeRoundTrip :: Handshake -> Bool
handshakeRoundTrip h = decode (encode h) == h

instance Arbitrary Handshake where
    arbitrary = do
        [resByte0, resByte1, resByte2, resByte3, resByte4, resByte5, resByte6,
         resByte7] <- replicateM 8 arbitrary
        infoHash <- B.pack <$> vectorOf 20 arbitrary
        peerId <- B.pack <$> vectorOf 20 arbitrary
        return Handshake {
            hResByte0 = resByte0, hResByte1 = resByte1, hResByte2 = resByte2,
            hResByte3 = resByte3, hResByte4 = resByte4, hResByte5 = resByte5,
            hResByte6 = resByte6, hResByte7 = resByte7,
            hInfoHash = infoHash,
            hPeerId = peerId
            }

peerMsgRoundTrip :: PeerMsg -> Bool
peerMsgRoundTrip p = decode (encode p) == p

instance Arbitrary PeerMsg where
    arbitrary = oneof [
        return Choke,
        return Unchoke,
        return Interested,
        return NotInterested,
        Have <$> arbitrary,
        Bitfield <$> arbitrary,
        Request <$> arbitrary <*> arbitrary <*> arbitrary,
        Piece <$> arbitrary <*> arbitrary <*> arbitrary,
        Cancel <$> arbitrary <*> arbitrary <*> arbitrary
        ]

instance Applicative Gen where
    pure = return
    (<*>) = ap

-- Just like peerMsgRoundTrip, but in the context of decodeI
decodeI1ChunkRoundTrip :: (Arbitrary a, Binary a, Eq a) => a -> Bool
decodeI1ChunkRoundTrip x = x == decoded
    where
    bsIn = B.concat $ L.toChunks $ encode x
    decoded = runIdentity $ run $ runIdentity $ enumPure1Chunk (WrapBS bsIn)
        decodeI

decodeINChunkRoundTrip :: (Arbitrary a, Binary a, Eq a) =>
    a -> Positive Int -> Bool
decodeINChunkRoundTrip x (Positive n) = x == decoded
    where
    bsIn = B.concat $ L.toChunks $ encode x
    decoded = runIdentity $ run $ runIdentity $ enumPureNChunk (WrapBS bsIn) n
        decodeI

serializePeerMsgs :: [PeerMsg] -> B.ByteString
serializePeerMsgs = B.concat . L.toChunks . runPut . mapM_ serialize
    where
    serialize :: PeerMsg -> Put
    serialize msg = do
        let msg' = encode msg
        putWord32be $ fromIntegral $ L.length msg'
        putLazyByteString msg'

enumPeerMsgRoundTrip1Chunk :: [PeerMsg] -> Bool
enumPeerMsgRoundTrip1Chunk msgs = runIdentity $ do
    let iter = joinI $ enumPeerMsg stream2list
    iter' <- enumPure1Chunk
        (WrapBS $ serializePeerMsgs msgs)
        iter
    msgs' <- run iter'
    return $ msgs' == msgs

enumPeerMsgRoundTripNChunk :: [PeerMsg] -> Positive Int -> Bool
enumPeerMsgRoundTripNChunk msgs (Positive n) = runIdentity $ do
    let iter = joinI $ enumPeerMsg stream2list
    iter' <- enumPureNChunk
        (WrapBS $ serializePeerMsgs msgs)
        n
        iter
    msgs' <- run iter'
    return $ msgs' == msgs
