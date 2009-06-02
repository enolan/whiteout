{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.ArbitraryInstances where

import Control.Applicative
import qualified Data.ByteString as B
import Data.Int (Int32)
import Data.Word (Word8)
import Test.QuickCheck

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbitrary
    shrink s = case B.splitAt (B.length s `div` 2) s of
        (a, b) -> [a,b]

instance Arbitrary Word8 where
    arbitrary = elements [minBound..maxBound]

arbitraryPosInt32 :: Gen Int32
arbitraryPosInt32 = fromIntegral <$> (arbitrary :: Gen Int) `suchThat` (>=0)
