{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Internal.Peer.Messages (theTests) where

import Control.Applicative
import Data.Binary
import qualified Data.ByteString as B
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Test.ArbitraryInstances ()
import Internal.Peer.Messages

theTests :: Test
theTests = testGroup "Internal.Peer.Messages" [
    testProperty "handshakeRoundTrip" handshakeRoundTrip,
    testProperty "peerMsgRoundTrip" peerMsgRoundTrip
    ]

handshakeRoundTrip :: Handshake -> Bool
handshakeRoundTrip h = (decode $ encode h) == h

instance Arbitrary Handshake where
    arbitrary = do
        [resByte0, resByte1, resByte2, resByte3, resByte4, resByte5, resByte6,
         resByte7] <- sequence $ replicate 8 arbitrary
        infoHash <- B.pack <$> vectorOf 20 arbitrary
        peerId <- B.pack <$> vectorOf 20 arbitrary
        return $ Handshake {
            resByte0 = resByte0, resByte1 = resByte1, resByte2 = resByte2,
            resByte3 = resByte3, resByte4 = resByte4, resByte5 = resByte5,
            resByte6 = resByte6, resByte7 = resByte7,
            infoHash = infoHash,
            peerId = peerId
            }

peerMsgRoundTrip :: PeerMsg -> Bool
peerMsgRoundTrip p = (decode $ encode p) == p

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
    f <*> x = do
        f' <- f
        x' <- x
        return $ f' x'
