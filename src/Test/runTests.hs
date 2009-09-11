import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import Network.Socket
import System.Environment
import Test.Framework

import Network.Whiteout
import Test.Internal.BEncode
import Test.Internal.Peers.Handler.Messages
import Test.Network.Whiteout

tests :: [Test]
tests = [
    Test.Internal.BEncode.theTests,
    Test.Internal.Peers.Handler.Messages.theTests,
    Test.Network.Whiteout.theTests
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> defaultMain tests
        ("tf" : rest) -> defaultMainWithArgs tests rest
        ("seedout" : host : port : []) -> singleseed host port
        ["seedin"] -> seedin
        _ -> putStrLn usage

usage :: String
usage = unlines [
    "Usage is one of:",
    "  runTests",
    "  runTests tf tfoptions",
    "  runTests seedout host port",
    "  runTests seedin", "",
    "The first runs the QC & HUnit tests via test-framework with no options.",
    "",
    "The second does the same but with the arguments after \"tf\" passed to",
    "test-framework.",
    "",
    "The third connects to a running bt client on the given host and port",
    "and seeds the torrent in the file:",
    "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3.torrent",
    "",
    "The fourth starts seeding the same file, but doesn't do any announcing.",
    "You should launch another bt client and manually connect to localhost to",
    "test incoming connections. When we do downloading too this will be much",
    "more automated."
    ]

stripTracker :: Torrent -> Torrent
stripTracker tor = tor {tAnnounce = ""}

initBradSucks :: IO (Session, TorrentSt)
initBradSucks = do
    logChan <- newTChanIO
    logToConsole logChan
    sess <- initialize Nothing (Just logChan) 31337
    tor <- stripTracker <$> loadTorrentFromFile
        "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3.torrent"
    torst <- addTorrent sess tor
        "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3"
    return (sess,torst)

singleseed :: String -> String -> IO ()
singleseed host port = do
    (sess, torst) <- initBradSucks
    host' <- inet_addr host
    startTorrent sess torst
    atomically . addPeer sess torst host' . fromIntegral $ (read port :: Int)
    threadDelay $ 3*1000000 -- 3 seconds should be enough.

seedin :: IO ()
seedin = do
    (sess, torst) <- initBradSucks
    startTorrent sess torst
    putStrLn . unlines $ [
        "Listening for incoming connections on port 31337. Ctrl-C when you're",
        "satisfied."]
    forever $ threadDelay 1000000
