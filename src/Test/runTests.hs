import Control.Concurrent
import Control.Concurrent.STM.TChan
import System.Environment
import Test.Framework

import Network.Whiteout
import Test.Internal.BEncode
import Test.Internal.Peer.Messages
import Test.Network.Whiteout

tests :: [Test]
tests = [
    Test.Internal.BEncode.theTests,
    Test.Internal.Peer.Messages.theTests,
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
    "runTests",
    "runTests singleseed host port",
    "runTests tf tfoptions"]

singleseed :: String -> String -> IO ()
singleseed host port = do
    logChan <- newTChanIO
    logToConsole logChan
    sess <- initialize Nothing $ Just logChan
    tor <- loadTorrentFromFile
        "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3.torrent"
    torst <- addTorrent sess tor
        "test-data/01_-_Brad_Sucks_-_Dropping_out_of_School.mp3"
    addPeer sess torst host $ fromIntegral $ (read port :: Int)
    threadDelay $ 3*1000000 -- 3 seconds should be enough.
