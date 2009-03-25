module Internal.BEncode.Lexer where

import Data.Char

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

data Token
    = TDict
    | TList
    | TInt
    | TString ByteString
    | TNumber Int
    | TEnd
      deriving (Show,Eq)


lexer :: L.ByteString -> [Token]
lexer fs | L.null fs = []
lexer fs
    = case ch of
        'd' -> TDict : lexer rest
        'l' -> TList : lexer rest
        'i' -> TInt  : lexer rest
        'e' -> TEnd  : lexer rest
        '-' -> let (digits,rest') = L.span isDigit rest
                   number = read (L.unpack digits)
               in TNumber (-number) : lexer rest'
        _ | isDigit ch
              -> let (digits,rest') = L.span isDigit fs
                     number = read (L.unpack digits)
                 in if L.null rest'
                       then [TNumber number]
                       else case L.head rest' of
                              ':' -> let (str, rest'') = L.splitAt (fromIntegral number) (L.tail rest')
                                     in TString (BS.concat $ L.toChunks str) : lexer rest''
                              _ -> TNumber number : lexer rest'
          | otherwise -> error "Lexer error."
    where ch = L.head fs
          rest = L.tail fs

