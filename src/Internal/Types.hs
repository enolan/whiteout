{-# OPTIONS_GHC -fno-warn-orphans #-}
module Internal.Types
    (
    Session(..),
    TorrentSt(..),
    Torrent(..),
    Activity(..)
    ) where

import Control.Concurrent.STM
import Data.Array.IArray (Array)
import Data.ByteString (ByteString)
import qualified Data.Map as M

-- | A Whiteout session. Contains various internal state.
data Session = Session {
    -- | Map from infohashes to torrents.
    torrents :: TVar (M.Map ByteString TorrentSt)
    }

-- | The state of a torrent.
data TorrentSt = TorrentSt {
    torrent :: Torrent,
    path :: FilePath,
    -- | Is a given piece completed? For now (2009-05-24) this only reflects
    -- whether a piece's hash has been checked and found correct.
    completion :: TArray Integer Bool,
    activity :: TVar Activity
    }

-- | The static information about a torrent, i.e. that stored in a file named
-- @foo.torrent@.
data Torrent = Torrent {
    -- | The announce URL.
    announce :: ByteString,
    -- | The name of the top-level directory or the file if it is a single file
    -- torrent.
    name :: ByteString,
    -- | Length of a piece in bytes.
    pieceLen :: Int,
    -- | Map piece numbers to their SHA-1 hashes.
    pieceHashes :: Array Integer ByteString,
    -- | Either the length of the single file or a list of filenames and their
    -- lengths.
    files :: Either Integer [(Integer, FilePath)],
    -- | SHA-1 of the bencoded info dictionary.
    infohash :: ByteString
    } deriving (Show)

-- | What is being done with a torrent at a given moment.
data Activity =
    Stopped
  | Verifying
    deriving (Eq, Ord, Show)
