{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Internal.BEncode (theTests) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word (Word8)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
    (Arbitrary(..), Gen, elements, oneof, resize, sized)

import Internal.BEncode

theTests :: Test
theTests = testGroup "Internal.BEncode" [
    testProperty "checkPack" checkPack,
    testProperty "checkReadPack" checkReadPack
    ]

instance Arbitrary BEncode where
    arbitrary = sized genBEncode

    shrink (BInt i)    = map BInt $ shrink i
    shrink (BString s) = map BString $ shrink s
    shrink (BList l)   = map BList $ shrink l
    shrink (BDict d)   = map BDict $ shrink d

genBEncode :: Int -> Gen BEncode
genBEncode 0 = oneof
    [fmap BInt arbitrary,
     fmap BString arbitrary]
genBEncode s = oneof
    [fmap BInt arbitrary,
     fmap BString arbitrary,
     fmap BList (resize (s `div` 2) arbitrary),
     fmap BDict (resize (s `div` 2) arbitrary)]

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbitrary
    shrink s = [B.take 20 s, B.drop 20 s]

instance Arbitrary Word8 where
    arbitrary = elements [minBound..maxBound]

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (Map k v) where
    arbitrary = fmap M.fromList arbitrary
    shrink = map M.fromList . shrink . M.toList

checkPack :: BEncode -> Bool
checkPack x = (B.concat $ L.toChunks $ bPack x) `seq` True

checkReadPack :: BEncode -> Bool
checkReadPack x = (bRead $ bPack x) == Just x
