{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Internal.Peer (theTests) where

import Control.Applicative
import Data.Binary
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Internal.Peer
import Test.ArbitraryInstances (arbitraryPosInt32)

theTests :: Test.Framework.Test
theTests =
    testGroup "Internal.Peer" [
    testProperty "peerMsgRoundTrip" peerMsgRoundTrip
    ]

peerMsgRoundTrip :: PeerMsg -> Bool
peerMsgRoundTrip p = (decode $ encode p) == p

instance Arbitrary PeerMsg where
    arbitrary = oneof [
        return Choke,
        return Unchoke,
        return Interested,
        return NotInterested,
        Have <$> arbitraryPosInt32,
        Bitfield <$> arbitrary,
        Request <$>
            arbitraryPosInt32 <*> arbitraryPosInt32 <*> arbitraryPosInt32,
        Piece <$> arbitraryPosInt32 <*> arbitraryPosInt32 <*> arbitrary,
        Cancel <$> arbitraryPosInt32 <*> arbitraryPosInt32 <*> arbitraryPosInt32
        ]

instance Applicative Gen where
    pure = return
    f <*> x = do
        f' <- f
        x' <- x
        return $ f' x'
