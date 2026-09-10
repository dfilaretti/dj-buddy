module TrackCatalog
  ( Track (..),
    TrackCatalog,
    TrackId,
    addTrack,
    getAllTrackIds,
    getTrackWithId,
    keyJumpSuccessors,
    sameKeySuccessors,
  )
where

import Camelot (Key, compatibleKeys)
import GHC.Generics (Generic)
import Data.Yaml (FromJSON)

{---------------------------------------------------------------------------
  Data
----------------------------------------------------------------------------}

type TrackId = String

type TrackCatalog = [Track]

data Track
  = Track
  { id :: TrackId,
    title :: String,
    key :: Key
  }
  deriving (Eq, Generic)

{---------------------------------------------------------------------------
  Instances
----------------------------------------------------------------------------}

instance FromJSON Track

-- TODO: revise this; what is it doing exactly?
instance Show Track where
  show (Track id trackTitle trackKey') =
    "\"" ++ id ++ ": " ++ trackTitle ++ "\" (" ++ show trackKey' ++ ")"

{---------------------------------------------------------------------------
  Functions
----------------------------------------------------------------------------}

lookupById :: TrackId -> TrackCatalog -> [Track]
lookupById id_ = filter (\t -> t.id == id_)

hasTrack :: Track -> TrackCatalog -> Bool
hasTrack t ts =
    case lookupById t.id ts of
        [] -> False
        _  -> True

addTrack :: Track -> TrackCatalog -> TrackCatalog
addTrack t ts
    | hasTrack t ts = error $ "track id '" ++ t.id ++ "' already exists in track catalog!"
    | otherwise     = t:ts

getTracks :: (Track -> Bool) -> TrackCatalog -> [TrackId]
getTracks predicate ts =
  map (.id) $ filter predicate ts

getTracksWithKey :: Key -> TrackCatalog -> [TrackId]
getTracksWithKey desiredKey =
  getTracks ((== desiredKey) . (.key))

sameKeySuccessors :: TrackId -> TrackCatalog -> [TrackId]
sameKeySuccessors trackId catalog =
  maybe [] (filter (/= trackId) . (`getTracksWithKey` catalog)) (trackKey trackId catalog)

keyJumpSuccessors :: TrackId -> TrackCatalog -> [TrackId]
keyJumpSuccessors trackId catalog =
  maybe [] (flip getTracksWithKeys catalog . compatibleKeys) (trackKey trackId catalog)

getTracksWithKeys :: [Key] -> TrackCatalog -> [TrackId]
getTracksWithKeys keys catalog =
  concatMap (`getTracksWithKey` catalog) keys

trackKey :: TrackId -> TrackCatalog -> Maybe Key
trackKey trackId catalog = case lookupById trackId catalog of
    [] -> Nothing
    [t] -> Just (t.key)
    _ -> error "more than 1 track with same ID"

getTrackWithId :: TrackId -> TrackCatalog -> Maybe Track
getTrackWithId tid ts =
    case lookupById tid ts of
        [] -> Nothing
        [t] -> Just t
        _ -> error "more than 1 track with same ID!"

getAllTrackIds :: TrackCatalog -> [TrackId]
getAllTrackIds = map (.id)
