module Internal.Pieces
    (
    getPiece
    ) where

import Control.Applicative
import qualified Control.Exception as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.IO

import Internal.Types

-- | Get a piece from a torrent. Returns 'Nothing' if the piece number is out of
-- bounds.
getPiece :: TorrentSt -> Integer -> IO (Maybe ByteString)
getPiece torst piecenum = let
    tor = torrent torst
    offset = piecenum * (fromIntegral $ pieceLen $ torrent torst)
    in
        C.catch
            (case files $ torrent torst of
                Left len -> if offset > len
                    then return Nothing
                    else
                        withBinaryFile (path torst) ReadMode
                            (\h -> do
                                hSeek h AbsoluteSeek offset
                                Just <$> B.hGet h (pieceLen tor))
                Right _ -> return Nothing)
            (\(_ :: C.IOException) -> return Nothing)
