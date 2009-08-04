module Internal.BEncode
    (
    BEncode(..),
    bRead,
    bPack,
    -- * Helpers
    getInt,
    getString,
    getList,
    getDict
    )
where

import Data.Binary (Binary(..), Put)
import Data.Binary.Put (putByteString, runPut)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import qualified Text.Show.ByteString as SBS

import Internal.BEncode.Lexer ( Token (..), lexer )


type BParser a = GenParser Token () a

{- | The B-coding defines an abstract syntax tree given as a simple
     data type here
-}
data BEncode = BInt Integer
	     | BString B.ByteString
	     | BList [BEncode]
             | BDict (Map B.ByteString BEncode)
	       deriving (Eq, Ord, Show)

-- Source possition is pretty useless in BEncoded data. FIXME
updatePos :: (SourcePos -> Token -> [Token] -> SourcePos)
updatePos pos _ _ = pos

bToken :: Token -> BParser ()
bToken t = tokenPrim show updatePos fn
    where fn t' | t' == t = Just ()
          fn _ = Nothing

token' :: (Token -> Maybe a) -> BParser a
token' = tokenPrim show updatePos

tnumber :: BParser Integer
tnumber = token' fn
    where fn (TNumber i) = Just i
          fn _ = Nothing

tstring :: BParser B.ByteString
tstring = token' fn
    where fn (TString str) = Just str
          fn _ = Nothing

withToken :: Token -> BParser a -> BParser a
withToken tok
    = between (bToken tok) (bToken TEnd)

--------------------------------------------------------------
--------------------------------------------------------------

bInt :: BParser BEncode
bInt = withToken TInt $ fmap BInt tnumber

bString :: BParser BEncode
bString = fmap BString tstring

bList :: BParser BEncode
bList = withToken TList $ fmap BList (many bParse)

bDict :: BParser BEncode
bDict = withToken TDict $ fmap (BDict . Map.fromAscList) (many bAssocList)
    where bAssocList
              = do str <- tstring
                   value <- bParse
                   return (str,value)

bParse :: BParser BEncode
bParse = bDict <|> bList <|> bString <|> bInt

{- | bRead is a conversion routine. It assumes a B-coded string as input
     and attempts a parse of it into a BEncode data type
-}
bRead :: L.ByteString -> Maybe BEncode
bRead str = case lexer str of
    Nothing -> Nothing
    Just x -> case parse bParse "" x of
        Left _err -> Nothing
        Right b   -> if checkDictsValid b then Just b else Nothing

-- | Sanity check. Make sure we were given bencoded dictionaries in proper
--   order.
checkDictsValid :: BEncode -> Bool
checkDictsValid (BInt _)    = True
checkDictsValid (BString _) = True
checkDictsValid (BList xs)  = all checkDictsValid xs
checkDictsValid (BDict d)   = Map.valid d && (all checkDictsValid $ Map.elems d)

bPack :: BEncode -> L.ByteString
bPack = runPut . bPut

bPut :: BEncode -> Put
bPut (BInt i)    = put 'i' >> SBS.showp i >> put 'e'
bPut (BString s) = SBS.showp (B.length s) >> put ':' >> putByteString s
bPut (BList l)   = put 'l' >> mapM_ bPut l >> put 'e'
bPut (BDict d)   = put 'd' >>
                   mapM_ 
                     (\(k,v) -> bPut (BString k) >> bPut v)
                     (Map.toList d) >>
                   put 'e'

-- | The get* family of functions make writing code that extracts from BEncoded
-- data a little easier.
getInt :: BEncode -> Maybe Integer
getInt i = case i of
    BInt i' -> Just i'
    _       -> Nothing

getString :: BEncode -> Maybe B.ByteString
getString s = case s of
    BString s' -> Just s'
    _          -> Nothing

getList :: BEncode -> Maybe [BEncode]
getList l = case l of
    BList l' -> Just l'
    _        -> Nothing

getDict :: BEncode -> Maybe (Map B.ByteString BEncode)
getDict d = case d of
    BDict d' -> Just d'
    _        -> Nothing
