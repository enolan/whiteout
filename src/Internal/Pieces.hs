module Internal.Pieces
    (
    getPiece
    ) where

import Control.Applicative
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
    in case files $ torrent torst of
        Left len -> if offset > len
            then return Nothing
            else do
                -- TODO: handle pools
                h <- openBinaryFile (path torst) ReadMode
                hSeek h AbsoluteSeek offset
                piece <- Just <$> B.hGet h (pieceLen tor)
                hClose h
                return piece
        Right _ -> error "multifile reading not implemented."
