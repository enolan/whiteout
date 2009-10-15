module Internal.Logging where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
import System.IO

import Internal.Types

maybeLog :: Session -> LogLevel -> String -> STM ()
maybeLog sess level msg = case logChan sess of
    Nothing -> return ()
    Just logChan' -> writeTChan logChan' (level,msg)

-- | Directs the passed logging channel to console.
logToConsole :: TChan (LogLevel, String) -> IO ()
logToConsole chan = do
    forkIO $ forever $ atomically (readTChan chan) >>= print
    return ()

-- | Directs the passed logging channel to a given file. Overwrites the content
-- of the file.
logToFile :: FilePath -> TChan (LogLevel, String) -> IO ()
logToFile path chan = do
    forkIO $ withFile path WriteMode $ \h -> do
        hSetBuffering h LineBuffering
        forever $
            atomically (readTChan chan) >>= hPrint h
    return ()
