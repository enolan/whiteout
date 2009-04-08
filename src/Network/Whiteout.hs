module Network.Whiteout
    (
    Torrent(),
--    TorrentInfo(..),
    loadTorrent
    ) where

import Data.Array.IArray (Array, bounds, listArray)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Digest.SHA1 (Word160(..), hash)
import qualified Data.Map as M
import System.FilePath (joinPath)

import Internal.BEncode

-- |The type of torrents.
data Torrent = Torrent {
    -- |The announce URL.
    announce :: ByteString,
    -- |The name of the top-level directory or the file if it is a single file
    --  torrent.
    name :: ByteString,
    -- |Length of a piece in bytes.
    pieceLen :: Int,
    -- |Map piece numbers to their SHA-1 hashes.
    pieceHashes :: Array Integer Word160,
    -- |Either the length of the single file or a list of filenames and their
    --  lengths.
    files :: Either Integer [(Integer, FilePath)],
    -- |SHA-1 of the bencoded info dictionary.
    infohash :: Word160
    } deriving (Show)

-- |Load a torrent from a file.
loadTorrent :: FilePath -> IO (Maybe Torrent)
loadTorrent f = fmap (\x -> bRead x >>= toTorrent) $ LBS.readFile f

toTorrent :: BEncode -> Maybe Torrent
toTorrent benc = do
    dict <- getDict benc
    announce <- M.lookup "announce" dict >>= getString
    info <- M.lookup "info" dict >>= getDict
    let infohash = hash $ LBS.unpack $ bPack $ BDict info
    pieceLen <- M.lookup "piece length" info >>= getInt
    pieceHashes <- M.lookup "pieces" info >>= getString
    pieceHashes' <- extractHashes pieceHashes
    name <- M.lookup "name" info >>= getString
    files <- getFiles info
    (Just Torrent
        {announce = announce,
         name = name,
         pieceLen = fromIntegral pieceLen,
         pieceHashes =
            listArray
                (0,(fromIntegral $ BS.length pieceHashes `div` 20) - 1)
                pieceHashes',
         infohash = infohash,
         files = files
        }) >>= checkLength
    where
        --The get* could probably all be replaced with something using generics.
        getInt i = case i of
            BInt i' -> Just i'
            _       -> Nothing
        getString s = case s of
            BString s' -> Just s'
            _          -> Nothing
        getList l = case l of
            BList l' -> Just l'
            _        -> Nothing
        getDict d = case d of
            BDict d' -> Just d'
            _        -> Nothing
        extractHashes hs = if ((BS.length hs) `mod` 20) == 0
            then Just $ group20s $ BS.unpack hs
            else Nothing
        group20s (w1 : w2: w3: w4: w5:
                  w6 : w7: w8: w9:w10:
                  w11:w12:w13:w14:w15:
                  w16:w17:w18:w19:w20:ws)
                 = (Word160
                     (pack4w8inw32  w1  w2  w3  w4)
                     (pack4w8inw32  w5  w6  w7  w8)
                     (pack4w8inw32  w9 w10 w11 w12)
                     (pack4w8inw32 w13 w14 w15 w16)
                     (pack4w8inw32 w17 w18 w19 w20)) : group20s ws
        group20s [] = []
        group20s _  = error "group20s called with length whose length % 20 /= 0"
        pack4w8inw32 w1 w2 w3 w4 = let
            w1' = shift (fromIntegral w1) 24
            w2' = shift (fromIntegral w2) 16
            w3' = shift (fromIntegral w3) 8
            w4' = fromIntegral w4
            in w1' .|. w2' .|. w3' .|. w4'
        getFiles i = let
            length = M.lookup "length" i >>= getInt
            files  = M.lookup "files" i >>= getList
            in case (length, files) of
                (Just i  , Nothing) -> Just $ Left i
                (Nothing , Just fs) -> fmap Right $ mapM getFile fs
                (Just _  , Just _ ) -> Nothing
                (Nothing , Nothing) -> Nothing
        getFile :: BEncode -> Maybe (Integer, FilePath)
        getFile d = do
            d' <- getDict d
            length <- M.lookup "length" d' >>= getInt
            path <- M.lookup "path" d' >>= getList >>= mapM getString
            let path' = joinPath $ map BSC.unpack path
            Just (length,path')
        checkLength t = let
            len = either id (sum . map fst) $ files t
            numPieces = (snd (bounds $ pieceHashes t)) + 1
            numPieces' = 
                ceiling
                    ((fromIntegral len :: Double) / (fromIntegral $ pieceLen t))
            in if numPieces == numPieces'
                then Just t
                else Nothing
