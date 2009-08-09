module Internal.Types
    (
    Session(..),
    TorrentSt(..),
    Torrent(..),
    Activity(..),
    LogLevel(..),
    WhiteoutException(..),
    PieceNum
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Array.IArray (Array)
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.Set (Set)
import Data.Typeable
import Data.Word (Word32)
import Network.Socket (HostAddress, PortNumber)

import {-# SOURCE #-} Internal.Peers.Handler (PeerSt)

-- | A Whiteout session. Contains various internal state.
data Session = Session {
    -- | Map from infohashes to torrents.
    torrents :: TVar (M.Map ByteString TorrentSt),
    sPeerId :: ByteString,
    logChan :: Maybe (TChan (LogLevel, ByteString))
    }

-- | The state of a torrent.
data TorrentSt = TorrentSt {
    sTorrent :: Torrent,
    sPath :: FilePath,
    -- | Is a given piece completed? For now (2009-05-24) this only reflects
    -- whether a piece's hash has been checked and found correct.
    sCompletion :: TArray PieceNum Bool,
    sActivity :: TVar Activity,
    -- | Map from peerIds to peers.
    sPeers :: TVar (Set PeerSt),
    -- | How many connection attempts are in progress.
    sConnectionsInProgress :: TVar (Set ThreadId),
    sPotentialPeers :: TVar [(HostAddress, PortNumber)]
    }

-- | The static information about a torrent, i.e. that stored in a file named
-- @foo.torrent@.
data Torrent = Torrent {
    -- | The announce URL.
    tAnnounce :: ByteString,
    -- | The name of the top-level directory or the file if it is a single file
    -- torrent.
    tName :: ByteString,
    -- | Length of a piece in bytes.
    tPieceLen :: Int,
    -- | Map piece numbers to their SHA-1 hashes.
    tPieceHashes :: Array PieceNum ByteString,
    -- | Either the length of the single file or a list of filenames and their
    -- lengths.
    tFiles :: Either Integer [(Integer, FilePath)],
    -- | SHA-1 of the bencoded info dictionary.
    tInfohash :: ByteString
    } deriving (Show)

-- | What is being done with a torrent at a given moment.
data Activity =
    Stopped -- ^ Twiddling its thumbs.
  | Stopping
  -- ^ Closing its peer connections in preparation for thumb-twiddling.
  | Verifying
  -- ^ Checking the piece hashes.
  | Running
  -- ^ Actively seeking and sending pieces to peers.
    deriving (Eq, Ord, Show)

type PieceNum = Word32

data LogLevel = Debug -- ^ Messages of interest only for debugging Whiteout.
              | Low -- ^ Boring messages e.g. new peer connections.
              | Medium -- ^ Slightly less boring messages e.g. announces.
              | Critical
              -- ^ Problems which obstruct the primary functionality of Whiteout
              -- e.g. an error reading from disk.
    deriving (Show, Eq)

data WhiteoutException =
    CouldntParseTorrent
  | HTTPDownloadFailed
  | CouldntParseURL
  | BadState
  -- ^ Tried to perform some action on a torrent precluded by the current state
  -- of the torrent. E.g. Tried to verify a torrent that is already Verifying.
  | BadFiles
    deriving (Show, Typeable, Eq)

instance Exception WhiteoutException
