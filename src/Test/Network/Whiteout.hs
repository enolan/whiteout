module Test.Network.Whiteout (theTests) where

import Control.Applicative
import Control.Concurrent.STM
import Data.Array.IArray
import qualified Data.Map as M
import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Network.Whiteout

theTests :: Test.Framework.Test
theTests =
    testGroup "Network.Whiteout" [
        testGroup "verify" [
            testCase "singleFileShouldSucceed" verifySingleFileShouldSucceed,
            testCase "singleFileShouldFail" verifySingleFileShouldFail,
            testCase "multiFileShouldSucceed" verifyMultiFileShouldSucceed,
            testCase "multiFileShouldFail" verifyMultiFileShouldFail
        ]
    ]

addTorWithAssert :: Session -> Torrent -> FilePath -> IO TorrentSt
addTorWithAssert sess tor path = do
    res <- addTorrent sess tor path
    assertBool "adding failed" res
    torst <- M.lookup (tInfohash tor) <$> (atomically $ getActiveTorrents sess)
    assertBool "Couldn't find TorrentSt" $ isJust torst
    return $ fromJust torst

verifyGeneric :: FilePath -> FilePath -> Bool -> Assertion
verifyGeneric torpath datapath expectedResult = do
    sess <- initialize Nothing
    tor <- loadTorrentFromFile torpath
    torst <- addTorWithAssert sess tor datapath
    beginVerifyingTorrent torst
    atomically $ do
        activity <- getActivity torst
        case activity of
            Verifying -> retry
            Stopped -> return ()
    let (_, max) = bounds $ pieceHashes tor
    allExpected <- fmap and $ atomically
        (mapM (\i -> (==expectedResult) <$> isPieceComplete torst i) [0..max])
    assertBool "Some pieces were not verified as expected" allExpected

verifySingleFileShouldSucceed :: Assertion
verifySingleFileShouldSucceed = verifyGeneric
    "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3.torrent"
    "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3"
    True

verifySingleFileShouldFail :: Assertion
verifySingleFileShouldFail = verifyGeneric
    "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3.torrent"
    "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3.zeroes"
    False

verifyMultiFileShouldSucceed :: Assertion
verifyMultiFileShouldSucceed = verifyGeneric
    "test-data/larry lessig - code v2.torrent"
    "test-data/Larry Lessig - Code V2"
    True

verifyMultiFileShouldFail :: Assertion
verifyMultiFileShouldFail = verifyGeneric
    "test-data/larry lessig - code v2.torrent"
    "test-data/Larry Lessig - Code V2.zeroes"
    False
