-- |Reading and writing bencoded values.
module Bencode (
    BValue(..)
    ) where

import Data.Binary (Binary(..))
import Data.Binary.Get (Get(), getLazyByteString, lookAhead, skip)
import Data.Binary.Put (Put(), putLazyByteString)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char (digitToInt, isDigit)
import Data.Int (Int64)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Text.Show.ByteString as SBS

-- |The type of bencoded values.
data BValue =
    BString ByteString
  | BInt Integer
  | BDict (Map ByteString BValue)
  | BList [BValue]
    deriving (Eq, Show)

instance Binary BValue where
    put = putBValue
    get = getBValue

---------------------------------------
-- Getting
---------------------------------------

getBValue :: Get BValue
getBValue = do
    c <- get
    case c of
        x
            | any (==c) ['0'..'9'] -> fmap BString (getBString $ decToInt x)
            | c == 'i' -> fmap BInt getInt
            | c == 'd' -> fmap BDict getDict
            | c == 'l' -> fmap BList getList
        _ -> error "invalid bencoding"

getBString :: Int64 -> Get ByteString
getBString i = do
    next <- get
    if next == ':'
        then getLazyByteString i
        else getBString $ i*10+(decToInt next)

getInt :: Get Integer
getInt = do
    c <- lookAhead get
    if c == '-'
        then (skip 1) >> (fmap negate $ getInt' 0)
        else getInt' 0
getInt' :: Integer -> Get Integer
getInt' i = do
    next <- get
    if next == 'e'
        then return i
        else getInt' (i*10+(decToInt next))

getDict :: Get (Map ByteString BValue)
getDict = getDict' []

getDict' :: [(ByteString, BValue)] -> Get (Map ByteString BValue)
getDict' acc = do
    next <- get
    if next == 'e'
        then let ret = M.fromAscList $ reverse acc in
            if M.valid ret
                then return ret
                else error
                    "dictionary in wrong order or multiple values for a key"
        else do
            key <- getBString $ decToInt next
            val <- get
            getDict' $ (key,val):acc

getList :: Get [BValue]
getList = getList' []

getList' :: [BValue] -> Get [BValue]
getList' acc = do
    next <- lookAhead get
    if next == 'e'
        then skip 1 >> return (reverse acc)
        else getList' . (: acc) =<< get

---------------------------------------
-- Putting
---------------------------------------

putBValue :: BValue -> Put
putBValue (BString s) = do
    SBS.showp (BS.length s)
    put ':'
    putLazyByteString s
putBValue (BInt i) = do
    put 'i'
    SBS.showp i
    put 'e'
putBValue (BDict d) = do
    put 'd'
    putBDict $ M.toAscList d
    put 'e'
putBValue (BList bs) = put 'l' >> mapM_ putBValue bs >> put 'e'

putBDict :: [(ByteString, BValue)] -> Put
putBDict = mapM_
    (\(k,v) -> (putBValue $ BString k) >> putBValue v)

---------------------------------------
-- Utilities
---------------------------------------

-- Data.Char.digitToInt accepts hex digits too for some reason.
decToInt :: Num a => Char -> a
decToInt c = if isDigit c then fromIntegral $ digitToInt c else error msg
    where msg = "tried to convert '" ++ c : "' to a int, but isn't a digit."
