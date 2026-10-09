module Domain.MusicLibrary
  ( MusicLibrary (..),
    MusicLibraryError,
    insertTransition,
    isValidTransition,
    emptyMusicLibrary,
    getTrack,
    Track (..),
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
    TestedTransition (..),
    TestedTransitionsGraph,
    TestedTransitionsError,
    emptyGraph,
    getEdges,
    addEdge,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Domain.Camelot (Key, compatibleKeys)

type TrackId = String

type TrackCatalog = Map.Map TrackId Track

data Track = Track {title :: Text, key :: Key} deriving (Eq)

data TrackCatalogError = TrackIdAlreadyExists TrackId deriving (Show, Eq)

titleMatches :: Text -> Track -> Bool
titleMatches s t = T.isInfixOf (T.toLower s) (T.toLower t.title)

keyMatches :: Key -> Track -> Bool
keyMatches k t = k == t.key

instance Show Track where
  show (Track trackTitle trackKey') = "\"" ++ T.unpack trackTitle ++ "\" (" ++ show trackKey' ++ ")"

emptyCatalog :: TrackCatalog
emptyCatalog = Map.empty

addTrack :: (TrackId, Track) -> TrackCatalog -> Either TrackCatalogError TrackCatalog
addTrack (tid, t) ts = case Map.lookup tid ts of
  Nothing -> Right $ Map.insert tid t ts
  Just _ -> Left $ TrackIdAlreadyExists tid

getTracks :: (Track -> Bool) -> TrackCatalog -> [Track]
getTracks p = Map.elems . Map.filter p

getTrackIds :: (Track -> Bool) -> TrackCatalog -> [TrackId]
getTrackIds p = Map.keys . Map.filter p

getTracksWithKey :: Key -> TrackCatalog -> [TrackId]
getTracksWithKey desiredKey = getTrackIds ((== desiredKey) . (.key))

sameKeySuccessors :: TrackId -> TrackCatalog -> [TrackId]
sameKeySuccessors trackId catalog = maybe [] (filter (/= trackId) . (`getTracksWithKey` catalog)) (trackKey trackId catalog)

keyJumpSuccessors :: TrackId -> TrackCatalog -> [TrackId]
keyJumpSuccessors trackId catalog = maybe [] (flip getTracksWithKeys catalog . compatibleKeys) (trackKey trackId catalog)

harmonicSuccessors :: TrackId -> TrackCatalog -> [TrackId]
harmonicSuccessors trackId catalog = sameKeySuccessors trackId catalog ++ keyJumpSuccessors trackId catalog

getTracksWithKeys :: [Key] -> TrackCatalog -> [TrackId]
getTracksWithKeys keys catalog = concatMap (`getTracksWithKey` catalog) keys

trackKey :: TrackId -> TrackCatalog -> Maybe Key
trackKey trackId catalog = (.key) <$> Map.lookup trackId catalog

getTrackWithId :: TrackId -> TrackCatalog -> Maybe Track
getTrackWithId = Map.lookup

getAllTrackIds :: TrackCatalog -> [TrackId]
getAllTrackIds = Map.keys

type TestedTransitionsGraph = Map.Map TrackId [TestedTransition]

data UserRating = Nice | Omg deriving (Show, Eq, Ord)

data TestedTransition = TestedTransition {to :: TrackId, notes :: Maybe String} deriving (Show, Eq)

data TestedTransitionsError = TestedTransitionAlreadyExists TrackId TrackId deriving (Show, Eq)

emptyGraph :: TestedTransitionsGraph
emptyGraph = Map.empty

transExists :: TestedTransitionsGraph -> TrackId -> TrackId -> Bool
transExists g from to = any (\tt -> tt.to == to) (getEdges from g)

addEdge :: TrackId -> TestedTransition -> TestedTransitionsGraph -> Either TestedTransitionsError TestedTransitionsGraph
addEdge from t g
  | transExists g from t.to = Left $ TestedTransitionAlreadyExists from t.to
  | otherwise = Right $ Map.alter f from g
  where
    f x = case x of
      Nothing -> Just [t]
      Just ts -> Just (t : ts)

getEdges :: TrackId -> TestedTransitionsGraph -> [TestedTransition]
getEdges = Map.findWithDefault []

data MusicLibrary = MusicLibrary {trackCatalog :: TrackCatalog, transitionGraph :: TestedTransitionsGraph} deriving (Show)

data MusicLibraryError
  = InsertingMalformedTransition TrackId TestedTransition
  | TestedTransitionsError TestedTransitionsError
  deriving (Show, Eq)

emptyMusicLibrary :: MusicLibrary
emptyMusicLibrary = MusicLibrary {trackCatalog = emptyCatalog, transitionGraph = emptyGraph}

isValidTransition :: (TrackId, TestedTransition) -> MusicLibrary -> Bool
isValidTransition (tid, t) lib = and [tid /= t.to, tid `elem` validIds, t.to `elem` validIds]
  where
    validIds = getAllTrackIds lib.trackCatalog

insertTransition_ :: TrackId -> TestedTransition -> MusicLibrary -> Either MusicLibraryError MusicLibrary
insertTransition_ tid t lib = case addEdge tid t lib.transitionGraph of
  Left e -> Left $ TestedTransitionsError e
  Right g' -> Right $ MusicLibrary {trackCatalog = lib.trackCatalog, transitionGraph = g'}

insertTransition :: TrackId -> TestedTransition -> MusicLibrary -> Either MusicLibraryError MusicLibrary
insertTransition tid t lib
  | isValidTransition (tid, t) lib = insertTransition_ tid t lib
  | otherwise = Left (InsertingMalformedTransition tid t)

getTrack :: TrackId -> MusicLibrary -> Maybe Track
getTrack tid lib = getTrackWithId tid lib.trackCatalog
