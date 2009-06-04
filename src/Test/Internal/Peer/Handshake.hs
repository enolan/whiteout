module Test.Internal.Peer.Handshake (theTests) where

import Control.Applicative
import Data.Binary
import qualified Data.ByteString as B
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Test.ArbitraryInstances ()
import Internal.Peer.Handshake

theTests :: Test
theTests = testGroup "Internal.Peer.Handshake" [
    testProperty "roundTrip" roundTrip
    ]

roundTrip :: Handshake -> Bool
roundTrip h = (decode $ encode h) == h

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
