module Domain.TrackCatalog
  ( Track (..),
    TrackCatalog,
    TrackCatalogError,
    TrackId,
    addTrack,
    emptyCatalog,
    getAllTrackIds,
    getTrackWithId,
    keyJumpSuccessors,
    sameKeySuccessors,
    titleMatches,
    keyMatches,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Domain.Camelot (Key, compatibleKeys)

{---------------------------------------------------------------------------
  Data
----------------------------------------------------------------------------}

type TrackId = String

type TrackCatalog = Map.Map TrackId Track

data Track
  = Track
  { title :: Text,
    key :: Key
  }
  deriving (Eq)

data TrackCatalogError
  = TrackIdAlreadyExists TrackId
  deriving (Show, Eq)

{---------------------------------------------------------------------------
  Predicates
----------------------------------------------------------------------------}

titleMatches :: Text -> Track -> Bool
titleMatches s t =
  T.isInfixOf (T.toLower s) (T.toLower t.title)

keyMatches :: Key -> Track -> Bool
keyMatches k t = k == t.key

{---------------------------------------------------------------------------
  Instances
----------------------------------------------------------------------------}

instance Show Track where
  show (Track trackTitle trackKey') =
    "\"" ++ T.unpack trackTitle ++ "\" (" ++ show trackKey' ++ ")"

{---------------------------------------------------------------------------
  Functions
----------------------------------------------------------------------------}

emptyCatalog :: TrackCatalog
emptyCatalog = Map.empty

addTrack :: (TrackId, Track) -> TrackCatalog -> Either TrackCatalogError TrackCatalog
addTrack (tid, t) ts =
  case Map.lookup tid ts of
    Nothing -> Right $ Map.insert tid t ts
    Just _ -> Left $ TrackIdAlreadyExists tid

getTracks :: (Track -> Bool) -> TrackCatalog -> [Track]
getTracks p =
  Map.elems . Map.filter p

getTrackIds :: (Track -> Bool) -> TrackCatalog -> [TrackId]
getTrackIds p =
  Map.keys . Map.filter p

getTracksWithKey :: Key -> TrackCatalog -> [TrackId]
getTracksWithKey desiredKey =
  getTrackIds ((== desiredKey) . (.key))

sameKeySuccessors :: TrackId -> TrackCatalog -> [TrackId]
sameKeySuccessors trackId catalog =
  maybe [] (filter (/= trackId) . (`getTracksWithKey` catalog)) (trackKey trackId catalog)

keyJumpSuccessors :: TrackId -> TrackCatalog -> [TrackId]
keyJumpSuccessors trackId catalog =
  maybe [] (flip getTracksWithKeys catalog . compatibleKeys) (trackKey trackId catalog)

harmonicSuccessors :: TrackId -> TrackCatalog -> [TrackId]
harmonicSuccessors trackId catalog =
  sameKeySuccessors trackId catalog ++ keyJumpSuccessors trackId catalog

getTracksWithKeys :: [Key] -> TrackCatalog -> [TrackId]
getTracksWithKeys keys catalog =
  concatMap (`getTracksWithKey` catalog) keys

trackKey :: TrackId -> TrackCatalog -> Maybe Key
trackKey trackId catalog =
  (.key) <$> Map.lookup trackId catalog

getTrackWithId :: TrackId -> TrackCatalog -> Maybe Track
getTrackWithId = Map.lookup

getAllTrackIds :: TrackCatalog -> [TrackId]
getAllTrackIds = Map.keys
