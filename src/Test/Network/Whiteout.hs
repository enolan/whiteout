module Test.Network.Whiteout where

import Control.Concurrent.STM
import Data.Array.IArray
import Data.Digest.SHA1
import qualified Data.Map as M
import Data.Maybe
import Test.HUnit

import Network.Whiteout

verifySingleFile :: Assertion
verifySingleFile = do
    sess <- initialize
    tor <- loadTorrentFromFile
        "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3.torrent"
    assertBool "Torrent didn't load" $ isJust tor
    let tor' = fromJust tor
    res <- addTorrent sess tor'
        "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3"
    assertBool "Adding failed" res
    torrents <- atomically $ readLoadedTorrents sess
    let
        torst = M.lookup
            (Word160 958421318 1479285429 3749561784 3909306547 1811215688)
            torrents
    assertBool "Couldn't find TorrentSt" $ isJust torst
    let torst' = fromJust torst
    beginVerifyingTorrent torst'
    atomically $ do
        verified <- isTorrentVerified torst'
        if verified
            then return ()
            else retry
    let (_, max) = bounds $ pieceHashes tor'
    allGood <- fmap and $ atomically (mapM (isPieceComplete torst') [0..max])
    assertBool "Not all pieces passed verification" allGood
