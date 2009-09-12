module Internal.Peers
    (
    addPeer,
    peerListener,
    startTorrent,
    stopTorrent
    ) where

import Prelude hiding (mapM_)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad hiding (mapM_)
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (mapM_)
import qualified Data.Map as M
import Network.Socket
import System.IO.Unsafe

import qualified Internal.Announce as A
import Internal.Logging
import Internal.Peers.Handler
import Internal.Types

-- | Add a single peer to the queue. Mostly for testing.
addPeer :: Session -> TorrentSt -> HostAddress -> PortNumber -> STM ()
addPeer sess torst h p = do
    maybeLog sess Medium $ BC.concat
        ["Adding single peer: ",
         BC.pack . unsafePerformIO . inet_ntoa $ h,
         ":",
         BC.pack . show $ p
        ]
    peers <- readTVar . sPotentialPeers $ torst
    writeTVar (sPotentialPeers torst) $ (h,p) : peers

-- | Stop seeding a torrent. The torrent's 'Activity' will be 'Stopping' for a
-- moment, then will transition to 'Stopped'. Will throw 'BadState' if the
-- torrent is not 'Running' when this is called.
stopTorrent :: Session -> TorrentSt -> STM ()
stopTorrent sess torst = do
    maybeLog sess Medium . BC.concat $
        ["Stopping torrent \"", tName . sTorrent $ torst, "\""]
    activity <- readTVar . sActivity $ torst
    case activity of
        Running -> writeTVar (sActivity torst) Stopping
        _       -> throw BadState

-- | Start seeding a torrent. Will throw 'BadState' if the torrent is not
-- 'Stopped' when this is called.
startTorrent :: Session -> TorrentSt -> IO ()
startTorrent sess torst = do
    goAhead <- atomically $ do
        activity <- readTVar . sActivity $ torst
        case activity of
            Stopped -> writeTVar (sActivity torst) Running >> return True
            _ -> return False
    if goAhead
        then return ()
        else throwIO BadState
    atomically . maybeLog sess Medium . BC.concat $
        ["Starting torrent: \"", tName . sTorrent $ torst, "\""]
    announceHelper sess torst (Just A.AStarted)
    forkIO $ peerManager sess torst
    return ()

-- This is the peer manager. There is one peer manager for each running
-- torrent. The idiom in WaitThenDoStuff is waiting for some event/state with
-- STM's retry then doing some action and signalling whether to exit; e.g.
-- waiting for the announce timer to go off and announcing or waiting for the
-- Activity of the TorrentSt to be Stopping then killing all the connections.
-- We combine a prioritized list of WaitThenDoStuff with orElse to do
-- everything the peer manager needs to do.

type WaitThenDoStuff = Session -> TorrentSt -> STM (IO Bool)

peerManager :: Session -> TorrentSt -> IO ()
peerManager sess torst = do
    let alternatives = map (flip ($ sess) torst)
            [cleanup, announce, getNextPeer]
    exit <- join . atomically . foldr1 orElse $ alternatives
    if exit
        then return ()
        else peerManager sess torst

-- Check if stopTorrent has been called and if so clean up and exit.
cleanup :: WaitThenDoStuff
cleanup sess torst = do
    activity <- readTVar . sActivity $ torst
    case activity of
        Running -> retry
        Verifying -> error
            "Invariant broken, verifying with peer manager running."
        Stopped -> error
            "Invariant broken, stopped with peer manager running."
        Stopping -> return go
    where
    go = do
        peerThreadIds <- M.keys <$> (atomically $ readTVar $ sPeers torst)
        mapM_ killThread peerThreadIds
        atomically $ do
            allPeerThreadsAreDead <- M.null <$> readTVar (sPeers torst)
            if allPeerThreadsAreDead
                then writeTVar (sActivity torst) Stopped
                else retry
        announceHelper sess torst $ Just A.AStopped
        return True

-- If it's time to announce, announce.
announce :: WaitThenDoStuff
announce sess torst = do
    itsTime <- (readTVar $ sTimeToAnnounce torst) >>= readTVar
    if itsTime
        then return go
        else retry
    where
    go = do
        announceHelper sess torst Nothing
        return False

getNextPeer :: WaitThenDoStuff
getNextPeer sess torst = do
    peerConnections <- M.size <$> readTVar (sPeers torst)
    if peerConnections < 30
        then do
            potentialPeers <- readTVar . sPotentialPeers $ torst
            case potentialPeers of
                [] -> retry
                peer : peers -> do
                    writeTVar (sPotentialPeers torst) peers
                    return $ go peer
        else retry
    where
    go (h, p) = do
        connectToPeer sess torst $ SockAddrInet p h
        return False

-- | Do an announce and do the right thing with the results. Asynchronous.
announceHelper :: Session -> TorrentSt -> Maybe A.AEvent -> IO ()
announceHelper sess torst at = do
    -- Make sure announceHelper isn't called again while we're waiting for the
    -- tracker.
    atomically
        (newTVar False >>= writeTVar (sTimeToAnnounce torst))
    forkIO $ catches go handlers
    return ()
    where
    go = do
        (interval, peers) <- A.announce sess torst at
        tv <- genericRegisterDelay $ interval * 1000000
        atomically $ do
            writeTVar (sTimeToAnnounce torst) tv
            writeTVar (sPotentialPeers torst) peers
            maybeLog sess Medium . BC.concat $
                ["Announced successfully, tracker requested interval of ",
                BC.pack $ show interval, " seconds."]
    handlers = [Handler $ \(e :: ErrorCall) -> handleEx e,
                Handler $ \(e :: IOException) -> handleEx e]
    handleEx e = do
        tv <- genericRegisterDelay $ (120 * 1000000 :: Integer)
        -- Guess where I pulled 120 seconds from. Maybe we should do
        -- exponential backoff...
        atomically $ do
            maybeLog sess Critical . BC.concat $
                ["Error in announce : \"", BC.pack $ show e, "\""]
            writeTVar (sTimeToAnnounce torst) tv

-- | Generified version of registerDelay. Needed because on a 32-bit machine,
-- the maximum wait of a normal registerDelay is only 35 minutes.
genericRegisterDelay :: Integral a => a -> IO (TVar Bool)
genericRegisterDelay microsecs = do
    tv <- newTVarIO False
    forkIO $ go tv microsecs
    return tv
    where
    maxWait = maxBound :: Int
    go tv microsecs' = do
        if (fromIntegral maxWait) < microsecs'
            then do
                threadDelay maxWait
                go tv $ microsecs' - (fromIntegral maxWait)
            else do
                threadDelay $ fromIntegral microsecs'
                atomically $ writeTVar tv True
