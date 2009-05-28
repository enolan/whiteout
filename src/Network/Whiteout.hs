module Network.Whiteout
    (
-- *Torrents
    Torrent(..),
    loadTorrentFromFile,
    loadTorrentFromURL,
    LoadTorrentFromURLError(..),
    loadTorrent,
-- *Whiteout state
    Session(),
    TorrentSt(),
    torrent,
    path,
    initialize,
    close,
    getActiveTorrents,
    isPieceComplete,
    isBeingVerified,
    addTorrent,
    beginVerifyingTorrent
    ) where

import Data.Array.IArray ((!), bounds, listArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Digest.SHA1 (Word160(..), hash)
import qualified Data.Map as M
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Network.URI (parseURI)
import Network.HTTP
    (Response(..), RequestMethod(..), mkRequest, simpleHTTP)
import System.Directory
    (Permissions(..), doesDirectoryExist, doesFileExist, getPermissions)
import System.FilePath ((</>), joinPath)
import System.IO

import Internal.BEncode
import Internal.Pieces
import Internal.Types


-- | Load a torrent from a file. Returns 'Nothing' if the file doesn't contain a
-- valid torrent. Throws an exception if the file can't be opened.
loadTorrentFromFile :: FilePath -> IO (Maybe Torrent)
loadTorrentFromFile = fmap loadTorrent . LBS.readFile

-- | Load a torrent from a URL.
loadTorrentFromURL ::
    String ->
    IO (Either LoadTorrentFromURLError Torrent)
loadTorrentFromURL u = do
    case parseURI u of
        Just uri' -> do
            let req = mkRequest GET uri'
            res <- simpleHTTP req
            case res of
                Left _  -> return $ Left DownloadFailed
                Right r -> return $ case loadTorrent $ rspBody r of
                    Just t  -> Right t
                    Nothing -> Left NotATorrent
        Nothing   -> return $ Left URLInvalid

-- | Things that could go wrong downloading and loading a torrent.
data LoadTorrentFromURLError =
    -- | Download failed.
      DownloadFailed
    -- | URL was invalid.
    | URLInvalid
    -- | Download succeeded, but what we got was not a torrent.
    | NotATorrent
    deriving (Show, Eq)

-- | Load a torrent from a 'LBS.ByteString'. Returns 'Nothing' if the parameter
-- is not a valid torrent.
loadTorrent :: LBS.ByteString -> Maybe Torrent
loadTorrent bs = bRead bs >>= toTorrent

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
        extractHashes hs = if (BS.length hs `mod` 20) == 0
            then Just $ group20s $ BS.unpack hs
            else Nothing
        group20s (w1 : w2: w3: w4: w5:
                  w6 : w7: w8: w9:w10:
                  w11:w12:w13:w14:w15:
                  w16:w17:w18:w19:w20:ws)
                 = Word160
                     (pack4w8inw32  w1  w2  w3  w4)
                     (pack4w8inw32  w5  w6  w7  w8)
                     (pack4w8inw32  w9 w10 w11 w12)
                     (pack4w8inw32 w13 w14 w15 w16)
                     (pack4w8inw32 w17 w18 w19 w20) : group20s ws
        group20s [] = []
        group20s _  = error "group20s called with length % 20 /= 0"
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
            numPieces = snd (bounds $ pieceHashes t) + 1
            numPieces' = 
                ceiling
                    ((fromIntegral len :: Double) / (fromIntegral $ pieceLen t))
            in if numPieces == numPieces'
                then Just t
                else Nothing

-- This should eventually take more arguments. At least a port to listen on.
initialize :: IO Session
initialize = atomically $ do
    torrents <- newTVar M.empty
    return Session { torrents = torrents }

-- | Clean up after ourselves, closing file handles, ending connections, etc.
-- Run this before exiting.
close :: Session -> IO ()
close _ = return ()

-- | Get the currently active torrents, keyed by infohash. A torrent is active
-- as long as it has been 'addTorrent'ed; one can be simultaneous active and
-- stopped - ready to go but not actually doing anything yet.
getActiveTorrents :: Session -> STM (M.Map Word160 TorrentSt)
getActiveTorrents s = readTVar $ torrents s

-- | Is a given piece complete?
isPieceComplete :: TorrentSt -> Integer -> STM Bool
isPieceComplete torst = readArray (completion torst)

-- | Is the given torrent being verified currently?
isBeingVerified :: TorrentSt -> STM Bool
isBeingVerified = readTVar . verifying

-- | Add a torrent to a running session for seeding/checking. Since we only
-- support seeding at present, this requires the files be in place and of the
-- correct size. Returns 'True' on success.
addTorrent :: Session -> Torrent -> FilePath -> IO Bool
addTorrent sess tor path = case files tor of
    Left len -> do
        ok <- checkFile (len,path)
        if ok
            then atomically addTorrent' >> return True
            else return False
    Right fs -> do
        e <- doesDirectoryExist path
        if e
            then do
                p <- getPermissions path
                if readable p
                    then do
                        ok <- fmap and $ mapM (checkFile . addprefix) fs
                        if ok
                            then atomically addTorrent' >> return True
                            else return False
                    else return False
            else return False
    where
        addprefix (l,p) = (l, path </> p)
        checkFile :: (Integer, FilePath) -> IO Bool
        checkFile (size, path) = do
            e <- doesFileExist path
            if e
                then do
                    p <- getPermissions path
                    if readable p
                        then do
                            h <- openBinaryFile path ReadMode
                            size' <- hFileSize h
                            hClose h
                            if size == size'
                                then return True
                                else return False
                        else return False
                else return False
        addTorrent' :: STM ()
        addTorrent' = do
            torsts <- readTVar $ torrents sess
            completion <- newArray (bounds $ pieceHashes tor) False
            verifying <- newTVar False
            let
                torst = TorrentSt {
                    torrent = tor,
                    path = path,
                    completion = completion,
                    verifying = verifying
                    }
                torsts' = M.insert (infohash tor) torst torsts
            writeTVar (torrents sess) torsts'

-- | Verify the hashes of a torrent. See also 'isBeingVerified'.
beginVerifyingTorrent :: TorrentSt -> IO ()
beginVerifyingTorrent torst = do
    atomically $ writeTVar (verifying torst) True
    forkIO (verify 0)
    return ()
    where
        verify :: Integer -> IO ()
        verify piecenum = do
            piece <- getPiece torst piecenum
            case piece of
                Nothing -> error "Couldn't load a piece for verifying!"
                Just piece' -> do
                    let
                        expected = ((pieceHashes $ torrent torst) ! piecenum)
                        actual = hash $ BS.unpack piece'
                    if actual == expected
                        then atomically $
                            writeArray (completion torst) piecenum True
                        else atomically $
                            writeArray (completion torst) piecenum False
                    if piecenum == (snd $ bounds $ pieceHashes $ torrent torst)
                        then atomically $ writeTVar (verifying torst) False
                        else verify (piecenum+1)
