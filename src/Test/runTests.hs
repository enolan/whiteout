import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
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
        ("singleseed" : host : port : []) -> singleseed host port
        ("tf" : rest) -> defaultMainWithArgs tests rest
        _ -> putStrLn usage

usage :: String
usage = unlines [
    "Usage is one of:",
    "  runTests",
    "  runTests tf tfoptions",
    "  runTests singleseed host port", "",
    "The first runs the QC & HUnit tests via test-framework with no options.", "",
    "The second does the same but with the arguments after \"tf\" passed to",
    "test-framework.", "",
    "The third connects to a running bt client on the given host and port",
    "and seeds the torrent in the file:",
    "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3.torrent"
    ]

singleseed :: String -> String -> IO ()
singleseed host port = do
    logChan <- newTChanIO
    logToConsole logChan
    sess <- initialize Nothing (Just logChan) 31337
    tor <- loadTorrentFromFile
        "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3.torrent"
    torst <- addTorrent sess tor
        "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3"
    host' <- inet_addr host
    startTorrent sess torst
    atomically . addPeer sess torst host' . fromIntegral $ (read port :: Int)
    threadDelay $ 3*1000000 -- 3 seconds should be enough.
