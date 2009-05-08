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

-- |A Whiteout session. This is used for keeping track of currently open
--  torrents.
data Session = Session {
    -- |Map from infohashes to torrents.
    torrents :: TVar (M.Map Word160 TorrentSt)
    }

-- This should be done in Data.Digest.SHA1, but isn't for whatever reason.
-- We need it for the above Map.
deriving instance Ord Word160

-- |The state of a torrent.
data TorrentSt = TorrentSt {
    torrent :: Torrent,
    path :: FilePath
    }
    deriving Show

-- |The type of torrents.
data Torrent = Torrent {
    -- |The announce URL.
    announce :: ByteString,
    -- |The name of the top-level directory or the file if it is a single file
    --  torrent.
    name :: ByteString,
    -- |Length of a piece in bytes.
    pieceLen :: Int,
    -- |Map piece numbers to their SHA-1 hashes.
    pieceHashes :: Array Integer Word160,
    -- |Either the length of the single file or a list of filenames and their
    --  lengths.
    files :: Either Integer [(Integer, FilePath)],
    -- |SHA-1 of the bencoded info dictionary.
    infohash :: Word160
    } deriving (Show)
