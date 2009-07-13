{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.ArbitraryInstances where

import qualified Data.ByteString as B
import Data.Word (Word8, Word32)
import System.Random
import Test.QuickCheck

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbitrary
    shrink s = case B.splitAt (B.length s `div` 2) s of
        (a, b) -> [a,b]

instance Arbitrary Word8 where
    arbitrary = choose (minBound, maxBound)

instance Random Word8 where
    randomR (low, high) g =
        case randomR (fromIntegral low :: Integer, fromIntegral high) g of
            (val, gen) -> (fromIntegral val, gen)
    random = randomR (minBound, maxBound)

instance Arbitrary Word32 where
    arbitrary = choose (minBound, maxBound)

instance Random Word32 where
    randomR (low, high) g =
        case randomR (fromIntegral low :: Integer, fromIntegral high) g of
            (val, gen) -> (fromIntegral val, gen)
    random = randomR (minBound, maxBound)
