module Internal.BEncode.Lexer where

import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as L

data Token
    = TDict
    | TList
    | TInt
    | TString ByteString
    | TNumber Integer
    | TEnd
      deriving (Show,Eq)


lexer :: L.ByteString -> Maybe [Token]
lexer fs | L.null fs = Just []
lexer fs
    = case ch of
        'd' -> fmap (TDict :) (lexer rest)
        'l' -> fmap (TList :) (lexer rest)
        'i' -> fmap (TInt  :) (lexer rest)
        'e' -> fmap (TEnd  :) (lexer rest)
        '-' -> let (digits,rest') = L.span isDigit rest
                   number = read (L.unpack digits)
               in fmap (TNumber (-number) :) (lexer rest')
        _ | isDigit ch
              -> let (digits,rest') = L.span isDigit fs
                     number = read (L.unpack digits)
                 in if L.null rest'
                       then Just [TNumber number]
                       else case L.head rest' of
                              ':' -> let (str, rest'') = L.splitAt (fromIntegral number) (L.tail rest')
                                     in fmap (TString (BS.concat $ L.toChunks str) :) (lexer rest'')
                              _ -> fmap (TNumber number :) (lexer rest')
          | otherwise -> Nothing
    where ch = L.head fs
          rest = L.tail fs
