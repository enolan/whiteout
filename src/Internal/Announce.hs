module Internal.Announce
    (
    AEvent,
    announce
    )
    where

import Control.Applicative
import Control.Concurrent.STM (atomically)
import Data.Binary.Get
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Maybe (maybe)
import Network.HTTP
import Network.Socket
import Network.URI

import Internal.BEncode
import Internal.Logging
import Internal.Peers
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

-- | Announce to the tracker and add the resultant peers. Returns the tracker's
-- requested announce interval. May throw ErrorCalled exceptions.
announce ::
    Session -> TorrentSt -> (Maybe AEvent) -> IO Integer
announce sess torst at = do
    atomically . maybeLog sess Medium $ BC.concat
        ["Announcing torrent \"",
        tName . sTorrent $ torst,
        "\", with URI \"",
        BC.pack uri,
        "\""
        ]
    case parseURI uri of
        Just uri' -> do
            let req = mkRequest GET uri'
            res <- simpleHTTP req
            case res of
                Left _  -> error "Eep, download failed in announce"
                Right r -> case bRead (rspBody r) of
                    Nothing -> error "couldn't decode response!"
                    Just x -> case decodeAnnounceResp x of
                        Left Nothing -> error "Error decoding announce response."
                        Left (Just err) -> error $
                            "Got error from tracker: " ++ BC.unpack err
                        Right (AnnounceResp
                            {interval = interval', peers = peers'}) -> do
                            atomically $ setPeerList torst peers'
                            return interval'
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
        ("compact", "1")
        -- Request packed address + port info, rather than a bencoded list.
        ] ++ (maybe [] (\at'-> [("event", show at')]) at)

data AnnounceResp = AnnounceResp
    {
    interval :: Integer, -- ^ Seconds until we should next announce.
    peers :: [(HostAddress, PortNumber)]
    }
    deriving Show

-- | Turns a bencoded announce response into an AnnounceResp or, on failure,
-- an error message if we got one.
decodeAnnounceResp :: BEncode -> Either (Maybe BC.ByteString) AnnounceResp
decodeAnnounceResp annResp = case getDict annResp of
    Nothing -> Left Nothing
    Just annResp' -> case M.lookup "failure reason" annResp' >>= getString of
        Just reason -> Left (Just reason)
        Nothing -> case decodeAnnounceResp' annResp' of
            Nothing -> Left Nothing
            Just annResp'' -> Right annResp''

decodeAnnounceResp' :: M.Map BC.ByteString BEncode -> Maybe AnnounceResp
decodeAnnounceResp' dict = do
    interval' <- M.lookup "interval" dict >>= getInt
    peers' <- M.lookup "peers" dict
    peers'' <- case peers' of
        BString packed -> if ((BC.length packed) `mod` 6) == 0
            then Just . unpackPeers $ packed
            else Nothing
        BDict _unpacked -> Nothing
        -- I don't feel like supporting the dictionary model and I haven't
        -- seen a tracker that won't send packed peer info yet. When this bites
        -- you, chalk it up to my laziness and general failure as a human being.
        _ -> Nothing
    return $ AnnounceResp
        {
        interval = interval',
        peers = peers''
        }

unpackPeers :: BC.ByteString -> [(HostAddress, PortNumber)]
unpackPeers bs = runGet (unpackPeers' []) $ L.fromChunks [bs]

unpackPeers' :: [(HostAddress, PortNumber)] -> Get [(HostAddress, PortNumber)]
unpackPeers' acc = do
    weredone <- isEmpty
    if weredone
        then return . reverse $ acc
        else do
        -- The address and port are in network order already, so we leave them
        -- as they are.
            theiraddr <- getWord32host
            theirport <- PortNum <$> getWord16host
            unpackPeers' $ (theiraddr, theirport) : acc
