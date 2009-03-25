{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Internal.BEncode where

import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word (Word8)
import Test.QuickCheck(Arbitrary(..), elements, frequency)

import Internal.BEncode

instance Arbitrary BEncode where
    arbitrary = frequency [
        (20, fmap BInt arbitrary),
        (20, fmap BString arbitrary),
        (1, fmap BList arbitrary),
        (1, fmap BDict arbitrary)]

    shrink (BInt i)    = map BInt $ shrink i
    shrink (BString s) = map BString $ shrink s
    shrink (BList l)   = map BList $ shrink l
    shrink (BDict d)   = map BDict $ shrink d

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbitrary
    shrink s = [B.take 20 s, B.drop 20 s]

instance Arbitrary Word8 where
    arbitrary = elements [minBound..maxBound]

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (Map k v) where
    arbitrary = fmap M.fromList arbitrary
    shrink m = map M.fromList $ shrink $ M.toList m

checkReadPack :: BEncode -> Bool
checkReadPack x = (bRead $ bPack x) == (Just x)
