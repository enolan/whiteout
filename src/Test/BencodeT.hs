module Test.BencodeT where

import Data.Binary (decode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word (Word8)
import Test.QuickCheck(Arbitrary(..), elements, frequency)

import Bencode

instance Arbitrary BValue where
    arbitrary = frequency [
        (1, fmap BString arbitrary),
        (1, fmap BInt arbitrary),
        (2, fmap BDict arbitrary),
        (2, fmap BList arbitrary)]

    shrink (BString s) = map BString $ shrink s
    shrink (BInt i)    = map BInt $ shrink i
    shrink (BDict d)   = map BDict $ shrink d
    shrink (BList l)   = map BList $ shrink l

instance Arbitrary ByteString where
    arbitrary = fmap LBS.pack arbitrary
    shrink s = [LBS.take 20 s, LBS.drop 20 s]

instance Arbitrary Word8 where
    arbitrary = elements [minBound..maxBound]

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (Map k v) where
    arbitrary = fmap M.fromList arbitrary
    shrink m = map M.fromList $ shrink $ M.toList m

checkGetPut :: BValue -> Bool
checkGetPut x = (decode $ encode x) == x
