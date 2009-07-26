module Internal.Announce
    (
    AEvent,
    announce
    )
    where

import qualified Data.ByteString.Char8 as BC
import Data.Maybe (maybe)
import Network.HTTP
import Network.URI

import Internal.Types

-- | Kinds of events.
data AEvent = AStarted
            -- ^ The first announce of a given torrent in a given session
            | ACompleted
            -- ^ Send when we finish downloading.
            | AStopped
            -- ^ Send when we leave a swarm.
    deriving Eq

instance Show AEvent where
    show AStarted    = "started"
    show ACompleted  = "completed"
    show AStopped    = "stopped"

announce :: Session -> TorrentSt -> (Maybe AEvent) -> IO BC.ByteString
announce sess torst at = do
    print uri
    case parseURI uri of
        Just uri' -> do
            let req = mkRequest GET uri'
            res <- simpleHTTP req
            case res of
                Left _  -> error "Eep, download failed in announce"
                Right r -> return $ rspBody r
        Nothing -> error "couldn't parse URI in announce"
    where
    uri = BC.unpack (tAnnounce $ sTorrent torst) ++ "?" ++
        mkAnnounceQString sess torst at

mkAnnounceQString :: Session -> TorrentSt -> (Maybe AEvent) -> String
mkAnnounceQString sess torst at = urlEncodeVars vars
    where
    vars = [
        ("info_hash", BC.unpack $ tInfohash $ sTorrent torst),
        ("peer_id", BC.unpack $ sPeerId sess),
        -- FIXME port, uploaded
        ("port", "55555"),
        ("uploaded", "0"),
        ("downloaded", "0"),
        ("left", "0"),
        ("compact", "1") -- Packed address + port info.
        ] ++ (maybe [] (\at'-> [("event", show at')]) at)
