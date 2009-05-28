module Test.Network.Whiteout where

import Control.Applicative
import Control.Concurrent.STM
import Data.Array.IArray
import qualified Data.Map as M
import Data.Maybe
import Test.HUnit

import Network.Whiteout

loadTorWithAssert :: FilePath -> IO Torrent
loadTorWithAssert path = do
    tor <- loadTorrentFromFile path
    assertBool "Torrent didn't load" $ isJust tor
    return $ fromJust tor

addTorWithAssert :: Session -> Torrent -> FilePath -> IO TorrentSt
addTorWithAssert sess tor path = do
    res <- addTorrent sess tor path
    assertBool "adding failed" res
    torst <- M.lookup (infohash tor) <$> (atomically $ getActiveTorrents sess)
    assertBool "Couldn't find TorrentSt" $ isJust torst
    return $ fromJust torst

verifySingleFileShouldSucceed :: Assertion
verifySingleFileShouldSucceed = do
    sess <- initialize
    tor <- loadTorWithAssert "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3.torrent"
    torst <- addTorWithAssert sess tor "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3"
    beginVerifyingTorrent torst
    atomically $ do
        activity <- getActivity torst
        if activity == Verifying
            then retry
            else return ()
    let (_, max) = bounds $ pieceHashes tor
    allGood <- fmap and $ atomically (mapM (isPieceComplete torst) [0..max])
    assertBool "Not all pieces passed verification" allGood

verifySingleFileShouldFail :: Assertion
verifySingleFileShouldFail = do
    sess <- initialize
    tor <- loadTorWithAssert "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3.torrent"
    torst <- addTorWithAssert sess tor "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3.zeroes"
    beginVerifyingTorrent torst
    atomically $ do
        activity <- getActivity torst
        if activity == Verifying
            then retry
            else return ()
    let (_, max) = bounds $ pieceHashes tor
    allBad <- fmap (and . map not) $ atomically (mapM (isPieceComplete torst) [0..max])
    assertBool "Some pieces passed verification" allBad
