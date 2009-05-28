{-# OPTIONS_GHC -fno-warn-orphans #-}
module Internal.Types
    (
    Session(..),
    TorrentSt(..),
    Torrent(..)
    ) where

import Control.Concurrent.STM
import Data.Array.IArray (Array)
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.Digest.SHA1 (Word160(..))

-- | A Whiteout session. Contains various internal state.
data Session = Session {
    -- | Map from infohashes to torrents.
    torrents :: TVar (M.Map Word160 TorrentSt)
    }

-- This should be done in Data.Digest.SHA1, but isn't for whatever reason.
-- We need it for the above Map.
deriving instance Ord Word160

-- | The state of a torrent.
data TorrentSt = TorrentSt {
    torrent :: Torrent,
    path :: FilePath,
    -- | Is a given piece completed? For now (2009-05-24) this only reflects
    -- whether a piece's hash has been checked and found correct.
    completion :: TArray Integer Bool,
    verifying :: TVar Bool
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
    pieceHashes :: Array Integer Word160,
    -- | Either the length of the single file or a list of filenames and their
    -- lengths.
    files :: Either Integer [(Integer, FilePath)],
    -- | SHA-1 of the bencoded info dictionary.
    infohash :: Word160
    } deriving (Show)
