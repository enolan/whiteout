module Internal.Peer
    (
    addPeer
    ) where

import Control.Concurrent (forkIO)
import Data.Binary
import Network.Socket
import qualified Network.Socket.ByteString as SB
import qualified Network.Socket.ByteString.Lazy as SBL

import Internal.Peer.Messages
import Internal.Types

-- | Connect to a new peer. Later this should add them to a queue and another
-- thread should empty the queue with a limit on the max open/half-open
-- connections.
addPeer :: Session -> TorrentSt -> String -> PortNumber -> IO ()
addPeer sess torst h p = (forkIO go) >> return ()
    where
        go = do
            s <- socket AF_INET Stream 0
            addr <- inet_addr h
            connect s $ SockAddrInet p addr
            sendHandshake sess torst s
            peerHandler torst s

peerHandler :: TorrentSt -> Socket -> IO ()
peerHandler torst s = do
    return ()

sendHandshake :: Session -> TorrentSt -> Socket -> IO ()
sendHandshake sess torst s = SBL.sendAll s $ encode Handshake {
    resByte0 = 0, resByte1 = 0, resByte2 = 0, resByte3 = 0, resByte4 = 0,
    resByte5 = 0, resByte6 = 0, resByte7 = 0,
    infoHash = infohash $ torrent torst,
    Internal.Peer.Messages.peerId = Internal.Types.peerId sess} -- Ugh.
