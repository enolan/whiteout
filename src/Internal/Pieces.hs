module Internal.Pieces
    (
    getPiece
    ) where

import Control.Applicative
import qualified Control.Exception as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.FilePath ((</>))
import System.IO

import Internal.Types

-- | Get a piece from a torrent. Returns 'Nothing' if the piece number is out of
-- bounds.
getPiece :: TorrentSt -> PieceNum -> IO (Maybe ByteString)
getPiece torst piecenum = let
    tor = torrent torst
    offset = (fromIntegral piecenum) * (fromIntegral $ pieceLen $ torrent torst)
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
                Right filelist ->
                    getPieceMultiFile
                        (pieceLen tor) (path torst) filelist offset)
            (\(_ :: C.IOException) -> return Nothing)

-- If this becomes a performance concern we could use an interval tree.
getPieceMultiFile ::
    Int -> FilePath -> [(Integer, FilePath)] -> Integer ->
    IO (Maybe ByteString)
getPieceMultiFile pieceSize prefix files offset =
    fmap (fmap B.concat) $ go files offset pieceSize
    where
    go :: [(Integer, FilePath)] -> Integer -> Int -> IO (Maybe [ByteString])
    go []               _      neededBytes = if neededBytes > pieceSize
        then return Nothing
        else return $ Just []
    go _                _      0           = return $ Just []
    go ((len, path):fs) offset neededBytes = if offset > len
        then go fs (offset-len) neededBytes
        else do
            chunk <- withBinaryFile (prefix </> path) ReadMode
                (\h -> do
                    hSeek h AbsoluteSeek offset
                    B.hGet h neededBytes)
            rest <- go fs 0 (neededBytes - (B.length chunk))
            return $ (chunk:) <$> rest
