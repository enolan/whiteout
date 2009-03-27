module Internal.BEncode
  (
   -- * Data types
   BEncode(..),  
   -- * Functions
   bRead,
   bShow,
   bPack
  )
where

import qualified Data.Map as Map
import Data.Map (Map)
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

import Internal.BEncode.Lexer ( Token (..), lexer )


type BParser a = GenParser Token () a

{- | The B-coding defines an abstract syntax tree given as a simple
     data type here
-}
data BEncode = BInt Integer
	     | BString ByteString
	     | BList [BEncode]
             | BDict (Map String BEncode)
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

tstring :: BParser ByteString
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
                   return (BS.unpack str,value)

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
	     Right b   -> Just b

-- | Render a BEncode structure to a B-coded string
bShow :: BEncode -> ShowS
bShow be = bShow' be
  where
    sc = showChar
    ss = showString
    sKV (k,v) = sString k (length k) . bShow' v
    sDict dict = foldr (.) id (map sKV (Map.toAscList dict)) 
    sList list = foldr (.) id (map bShow' list)
    sString str len = shows len . sc ':' . ss str
    bShow' b =
      case b of
        BInt i    -> sc 'i' . shows i . sc 'e'
        BString s -> sString (BS.unpack s) (BS.length s)
        BList bl  -> sc 'l' . sList bl . sc 'e'
        BDict bd  -> sc 'd' . sDict bd . sc 'e'

bPack :: BEncode -> L.ByteString
bPack be = L.fromChunks (bPack' be [])
    where intTag = BS.pack "i"
          colonTag = BS.pack ":"
          endTag = BS.pack "e"
          listTag = BS.pack "l"
          dictTag = BS.pack "d"
          sString :: ByteString -> [ByteString] -> [ByteString]
          sString s r = BS.pack (show (BS.length s)) : colonTag : s : r
          bPack' :: BEncode -> [ByteString] -> [ByteString]
          bPack' (BInt i) r = intTag : BS.pack (show i) : endTag : r
          bPack' (BString s) r = sString s r
          bPack' (BList bl) r = listTag : foldr bPack' (endTag : r) bl
          bPack' (BDict bd) r = dictTag : foldr (\(k,v) -> sString (BS.pack k) . bPack' v) (endTag : r) (Map.toAscList bd)
