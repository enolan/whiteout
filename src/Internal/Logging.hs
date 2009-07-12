module Internal.Logging where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
import qualified Data.ByteString as B

import Internal.Types

maybeLog :: Session -> LogLevel -> B.ByteString -> STM ()
maybeLog sess level msg = case logChan sess of
    Nothing -> return ()
    Just logChan' -> writeTChan logChan' (level,msg)

-- | Directs the passed logging channel to console.
logToConsole :: TChan (LogLevel, B.ByteString) -> IO ()
logToConsole chan = do
    forkIO $ forever $ (atomically $ readTChan chan) >>= print
    return ()
