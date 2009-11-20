module Test.Network.BitTorrent.Whiteout (theTests) where

import Control.Applicative
import Control.Concurrent.STM
import Data.Array.IArray
import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Network.BitTorrent.Whiteout

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

verifyGeneric :: FilePath -> FilePath -> Bool -> Assertion
verifyGeneric torpath datapath expectedResult = do
    sess <- initialize Nothing Nothing Nothing
    tor <- loadTorrentFromFile torpath
    torst <- addTorrent sess tor datapath
    beginVerifyingTorrent sess torst
    atomically $ do
        activity <- getActivity torst
        case activity of
            Verifying -> retry
            Stopped -> return ()
            Running -> error "Ghost started torrent."
            Stopping -> error
                "Ghost started torrent, then stopped it again. Spooooooky."
    let (0, maxPieceNum) = bounds $ tPieceHashes tor
    allExpected <- and <$> atomically
        (mapM
            (\i -> (==expectedResult) <$> isPieceComplete torst i)
            [0..maxPieceNum]
        )
    assertBool "Some pieces were not verified as expected" allExpected
    close sess

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
