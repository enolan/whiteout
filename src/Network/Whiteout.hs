module Network.Whiteout
    (
-- *Torrents
    Torrent(..),
    PieceNum,
    loadTorrentFromFile,
    loadTorrentFromURL,
    loadTorrent,
-- *Whiteout state
    Session(),
    TorrentSt(),
    Activity(..),
    sTorrent,
    sPath,
    LogLevel(..),
    logToConsole,
    logToFile,
    initialize,
    close,
    getActiveTorrents,
    isPieceComplete,
    getActivity,
-- *Operations on torrents
    addTorrent,
    beginVerifyingTorrent,
    addPeer,
-- *Miscellany
    WhiteoutException(..)
    ) where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception as C
import Control.Monad (replicateM)
import Data.Array.IArray ((!), bounds, listArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Typeable
import Network.URI (parseURI)
import Network.HTTP
    (Response(..), RequestMethod(..), mkRequest, simpleHTTP)
import System.Directory
    (Permissions(..), doesDirectoryExist, doesFileExist, getPermissions)
import System.FilePath ((</>), joinPath)
import System.IO
import System.Random

import Internal.BEncode
import Internal.Logging
import Internal.Peers (addPeer)
import Internal.Pieces
import Internal.Types


-- | Load a torrent from a file. May throw 'CouldntParseTorrent' or some
-- 'IOException' resulting from trying to read the file.
loadTorrentFromFile :: FilePath -> IO Torrent
loadTorrentFromFile = fmap loadTorrent . L.readFile

-- | Load a torrent from a URL. May throw 'CouldntParseTorrent',
-- 'HTTPDownloadFailed' or 'CouldntParseURL'.
loadTorrentFromURL :: String -> IO Torrent
loadTorrentFromURL u =
    case parseURI u of
        Just uri' -> do
            let req = mkRequest GET uri'
            res <- simpleHTTP req
            case res of
                Left _  -> throw HTTPDownloadFailed
                Right r -> return $ loadTorrent $ rspBody r
        Nothing   -> throw CouldntParseURL

-- | Load a torrent from a 'L.ByteString'. May throw a 'CouldntParseTorrent'
-- exception.
loadTorrent :: L.ByteString -> Torrent
loadTorrent bs = case bRead bs >>= toTorrent of
    Just x -> x
    Nothing -> throw CouldntParseTorrent

toTorrent :: BEncode -> Maybe Torrent
toTorrent benc = do
    dict <- getDict benc
    announce <- M.lookup "announce" dict >>= getString
    info <- M.lookup "info" dict >>= getDict
    let
        infohash = B.concat $ L.toChunks $ bytestringDigest $ sha1 $ bPack $
            BDict info
    pieceLen <- M.lookup "piece length" info >>= getInt
    pieceHashes <- M.lookup "pieces" info >>= getString
    pieceHashes' <- extractHashes pieceHashes
    name <- M.lookup "name" info >>= getString
    files <- getFiles info
    Just Torrent
        {tAnnounce = announce,
         tName = name,
         tPieceLen = fromIntegral pieceLen,
         tPieceHashes =
            listArray
                (0,(fromIntegral $ B.length pieceHashes `div` 20) - 1)
                pieceHashes',
         tInfohash = infohash,
         tFiles = files
        } >>= checkLength
    where
        extractHashes hs = if (B.length hs `mod` 20) == 0
            then Just $ groupHashes hs
            else Nothing
        groupHashes hs = if B.null hs
            then []
            else let (hash, rest) = B.splitAt 20 hs in hash : groupHashes rest
        getFiles i = let
            torLength = M.lookup "length" i >>= getInt
            files  = M.lookup "files" i >>= getList
            in case (torLength, files) of
                (Just l  , Nothing) -> Just $ Left l
                (Nothing , Just fs) -> fmap Right $ mapM getFile fs
                (Just _  , Just _ ) -> Nothing
                (Nothing , Nothing) -> Nothing
        getFile :: BEncode -> Maybe (Integer, FilePath)
        getFile d = do
            d' <- getDict d
            fLength <- M.lookup "length" d' >>= getInt
            path <- M.lookup "path" d' >>= getList >>= mapM getString
            let path' = joinPath $ map BC.unpack path
            Just (fLength,path')
        checkLength t = let
            len = either id (sum . map fst) $ tFiles t
            numPieces = snd (bounds $ tPieceHashes t) + 1
            numPieces' = 
                ceiling $
                    (fromIntegral len :: Double) / fromIntegral (tPieceLen t)
            in if numPieces == numPieces'
                then Just t
                else Nothing

-- This should eventually take more arguments. At least a port to listen on.
initialize :: Maybe (B.ByteString)
    -- ^ Your client name and version. Must be exactly two characters, followed
    -- by four numbers. E.g. Azureus uses AZ2060.
    --
    -- See <http://wiki.theory.org/BitTorrentSpecification#peer_id> for a
    -- directory. If you pass 'Nothing', we'll use WO and the whiteout version.
    -> Maybe (TChan (LogLevel, B.ByteString))
    -- ^ Logging channel.
    -> IO Session
initialize name ourLogChan = do
    -- Could use cabal to get our version number here...
    peerId <- genPeerId $ fromMaybe "WO0000" name 
    atomically $ do
        torrents' <- newTVar M.empty
        return Session
            { torrents = torrents', sPeerId = peerId, logChan = ourLogChan }

genPeerId :: B.ByteString -> IO B.ByteString
genPeerId nameandver = do
    randompart <- BC.pack <$> replicateM 12 (randomRIO ('0','9'))
    return $ B.concat ["-", nameandver, "-", randompart]

-- | Clean up after ourselves, closing file handles, ending connections, etc.
-- Run this before exiting.
close :: Session -> IO ()
close _ = return ()

-- | Get the currently active torrents, keyed by infohash. A torrent is active
-- as long as it has been 'addTorrent'ed; one can be simultaneous active and
-- stopped - ready to go but not actually doing anything yet.
getActiveTorrents :: Session -> STM (M.Map B.ByteString TorrentSt)
getActiveTorrents = readTVar . torrents

isPieceComplete :: TorrentSt -> PieceNum -> STM Bool
isPieceComplete = readArray . sCompletion

getActivity :: TorrentSt -> STM Activity
getActivity = readTVar . sActivity

-- | Add a torrent to a running session for seeding/checking. Since we only
-- support seeding at present, this requires the files be in place and of the
-- correct size. If this is not the case, throws 'BadFiles'. Returns the
-- TorrentSt added, for convenience.
addTorrent :: Session -> Torrent -> FilePath -> IO TorrentSt
addTorrent sess tor path = case tFiles tor of
    Left len -> do
        ok <- checkFile (len,path)
        if ok
            then atomically addTorrent'
            else throw BadFiles
    Right fs -> do
        e <- doesDirectoryExist path
        if e
            then do
                p <- getPermissions path
                if readable p
                    then do
                        ok <- and <$> mapM (checkFile . addprefix) fs
                        if ok
                            then atomically addTorrent'
                            else throw BadFiles
                    else throw BadFiles
            else throw BadFiles
    where
        addprefix (l,p) = (l, path </> p)
        checkFile :: (Integer, FilePath) -> IO Bool
        checkFile (size, path') = do
            e <- doesFileExist path'
            if e
                then do
                    p <- getPermissions path'
                    if readable p
                        then withBinaryFile path' ReadMode $ \h -> do
                            size' <- hFileSize h
                            if size == size'
                                then return True
                                else return False
                        else return False
                else return False
        addTorrent' :: STM TorrentSt
        addTorrent' = do
            torsts <- readTVar $ torrents sess
            completion <- newArray (bounds $ tPieceHashes tor) False
            activity <- newTVar Stopped
            peers <- newTVar M.empty
            let
                torst = TorrentSt {
                    sTorrent = tor,
                    sPath = path,
                    sCompletion = completion,
                    sActivity = activity,
                    sPeers = peers}
                torsts' = M.insert (tInfohash tor) torst torsts
            writeTVar (torrents sess) torsts'
            return torst

-- | Launch a thread to asynchronously verify the hashes of a torrent.
--
-- The torrent must be 'Stopped' before calling this, otherwise a 'BadState'
-- exception will be thrown. When the verifier thread starts, the torrent's
-- 'Activity' will be 'Verifying'; when it finishes it will set it back to
-- 'Stopped'.
beginVerifyingTorrent :: Session -> TorrentSt -> IO ()
beginVerifyingTorrent sess torst = do
    a <- atomically $ getActivity torst
    case a of
        Stopped -> do
            atomically $ writeTVar (sActivity torst) Verifying
            forkIO (verify 0)
            return ()
        _ -> throw BadState
    where
        verify :: PieceNum -> IO ()
        verify piecenum = do
            piece <- getPiece torst piecenum
            case piece of
                Nothing -> do
                    atomically $ writeTVar (sActivity torst) Stopped
                    atomically $ maybeLog sess Critical
                        "verifier thread: couldn't read a piece, aborting."
                Just piece' -> do
                    let
                        expected = (tPieceHashes $ sTorrent torst) ! piecenum
                        actual = B.concat $ L.toChunks $ bytestringDigest $
                            sha1 $ L.fromChunks [piece']
                    if actual == expected
                        then atomically $
                            writeArray (sCompletion torst) piecenum True
                        else atomically $
                            writeArray (sCompletion torst) piecenum False
                    if piecenum ==
                        (snd $ bounds $ tPieceHashes $ sTorrent torst)
                        then atomically $ writeTVar (sActivity torst) Stopped
                        else verify (piecenum+1)

data WhiteoutException =
    CouldntParseTorrent
  | HTTPDownloadFailed
  | CouldntParseURL
  | BadState
  -- ^ Tried to perform some action on a torrent precluded by the current state
  -- of the torrent. E.g. Tried to verify a torrent that is already Verifying.
  | BadFiles
    deriving (Show, Typeable, Eq)

instance Exception WhiteoutException
