-- |Reading and writing bencoded values.
module Bencode (
    BValue(..),
    decode,
    encode
    ) where

import Data.Binary (Binary(..))
import Data.Binary.Get (Get(), getLazyByteString, lookAhead, runGet)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char (digitToInt, isDigit)
import Data.Int (Int64)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Word (Word8)

-- |The type of bencoded values.
data BValue =
    BString ByteString
  | BInt Integer
  | BDict (Map ByteString BValue)
  | BList [BValue]
    deriving (Eq, Show)

-- |Decode a ByteString to a BValue
decode :: ByteString -> BValue
decode = runGet getBValue

getBValue :: Get BValue
getBValue = do
    c <- get
    case c of
        x
            | any (==c) ['0'..'9'] -> fmap BString (getBString $ decToInt x)
            | c == 'i' -> fmap BInt getInt
            | c == 'd' -> fmap BDict getDict
            | c == 'l' -> fmap BList getList

-- |Read a string with an accumulating parameter for the length.
getBString :: Int64 -> Get ByteString
getBString i = do
    next <- get
    if next == ':'
        then getLazyByteString i
        else getBString $ i*10+(decToInt next)

getInt :: Get Integer
getInt = get >>= getInt' . decToInt
getInt' :: Integer -> Get Integer
getInt' i = do
    next <- get
    if next == 'e'
        then return i
        else getInt' (i*10+(decToInt next))

getDict :: Get (Map ByteString BValue)
getDict = undefined

getList :: Get [BValue]
getList = undefined

encode = undefined

-- UTILITIES

-- Data.Char.digitToInt accepts hex digits too for some reason.
decToInt :: Num a => Char -> a
decToInt c = if isDigit c then fromIntegral $ digitToInt c else error msg
    where msg = "tried to convert" ++ c : " to a int, but isn't a digit."
