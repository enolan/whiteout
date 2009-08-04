-- This will be the entry point for the peer manager.
module Internal.Peers
    (
    addPeer,
    setPeerList,
    startTorrent
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever)
import Data.ByteString.Char8 as BC
import qualified Data.Set as S
import Network.Socket
import System.IO.Unsafe

import Internal.Logging
import Internal.Peers.Handler
import Internal.Types

-- | Set the list of potential peers for a torrent.
setPeerList :: TorrentSt -> [(HostAddress, PortNumber)] -> STM ()
setPeerList = writeTVar . sPotentialPeers

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

-- | Start seeding a torrent.
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
        ["Starting torrent: \"",
         tName . sTorrent $ torst,
         "\""
        ]
    forkIO . forever $ do
        (h,p) <- atomically $ do
            activePeers <- readTVar . sPeers $ torst
            connectionsInProgress <- readTVar . sConnectionsInProgress $ torst
            if (S.size activePeers < 30) && (connectionsInProgress < 10)
                then do
                    potentialPeers <- readTVar . sPotentialPeers $ torst
                    case potentialPeers of
                        [] -> retry
                        peer : peers -> do
                            writeTVar (sPotentialPeers torst) peers
                            writeTVar (sConnectionsInProgress torst) $
                                connectionsInProgress + 1
                            return peer
                else retry
        connectToPeer sess torst h p
    return ()
