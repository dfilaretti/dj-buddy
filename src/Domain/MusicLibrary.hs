module Domain.MusicLibrary
  ( MusicLibrary (..),
    MusicLibraryError,
    emptyMusicLibrary,
    Track (..),
    TrackId,
    addTrack,
    getTrackIds,
    filterLib,
    getPairs,
    getTrackWithId,
    getTracks,
    keyJumpSuccessors,
    withTransition_,
    sameKeySuccessors,
    titleMatches,
    keyMatches,
    TestedTransition (..),
    getEdges,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Domain.Camelot (Key, compatibleKeys)
import GHC.Generics (Generic)

{---------------------------------------------------------------------------
  Errors
----------------------------------------------------------------------------}

data MusicLibraryError
  = InsertingMalformedTransition TrackId TestedTransition
  | TrackIdAlreadyExists TrackId
  | TestedTransitionAlreadyExists TrackId TrackId
  | TransitionHasInvalidTrackId TrackId
  | TransitionHasSameSourceAndDest TrackId
  | InvalidTrackId TrackId
  deriving (Show, Eq)

{---------------------------------------------------------------------------
  Data types
----------------------------------------------------------------------------}

type TrackId = String

data Track = Track
  { title :: Text,
    key :: Key
  }
  deriving (Eq)

data TestedTransition = TestedTransition
  { notes :: Maybe String
  }
  deriving (Show, Eq)

data TrackNode = TrackNode
  { track :: Track,
    testedTransitions :: Map TrackId TestedTransition
  }

newtype MusicLibrary = MusicLibrary (Map TrackId TrackNode)

{---------------------------------------------------------------------------
  Functions (basic)
----------------------------------------------------------------------------}

mkTrackNode :: Track -> TrackNode
mkTrackNode t = (TrackNode t Map.empty)

getNode :: TrackId -> MusicLibrary -> Maybe TrackNode
getNode tid (MusicLibrary lib) = Map.lookup tid lib

withNode :: TrackId -> TrackNode -> MusicLibrary -> MusicLibrary
withNode tid node (MusicLibrary lib) =
  MusicLibrary $ Map.insert tid node lib

getTransition :: TrackNode -> TrackId -> Maybe TestedTransition
getTransition node tid = Map.lookup tid (node.testedTransitions)

hasTransition :: TrackNode -> TrackId -> Bool
hasTransition node = isJust . getTransition node

getTransition_ :: TrackId -> TrackId -> MusicLibrary -> Maybe TestedTransition
getTransition_ from to lib =
  do
    node <- getNode from lib
    getTransition node to

hasTransition_ :: TrackId -> TrackId -> MusicLibrary -> Bool
hasTransition_ from to = isJust . getTransition_ from to

withTransition :: TrackId -> TestedTransition -> TrackNode -> TrackNode
withTransition dest tdata node =
  TrackNode
    node.track
    (Map.insert dest tdata node.testedTransitions)

withTransition_ ::
  TrackId ->
  TrackId ->
  TestedTransition ->
  MusicLibrary ->
  Either MusicLibraryError MusicLibrary
withTransition_ from to info lib
  | from == to = Left $ TransitionHasSameSourceAndDest from
  | otherwise =
      case getNode from lib of
        Nothing -> Left $ TransitionHasInvalidTrackId from
        Just fromNode ->
          case getNode to lib of
            Nothing -> Left $ TransitionHasInvalidTrackId to
            Just _ -> Right $ withNode from updatedNode lib
          where
            updatedNode = withTransition to info fromNode

{---------------------------------------------------------------------------
  Functions
----------------------------------------------------------------------------}

instance Show Track where
  show (Track trackTitle trackKey') = "\"" ++ T.unpack trackTitle ++ "\" (" ++ show trackKey' ++ ")"

titleMatches :: Text -> Track -> Bool
titleMatches s t = T.isInfixOf (T.toLower s) (T.toLower t.title)

keyMatches :: Key -> Track -> Bool
keyMatches k t = k == t.key

hasTrack :: TrackId -> MusicLibrary -> Bool
hasTrack tid (MusicLibrary lib) = Map.member tid lib

addTrack ::
  (TrackId, Track) ->
  MusicLibrary ->
  Either MusicLibraryError MusicLibrary
addTrack (tid, t) (MusicLibrary ts)
  | Map.member tid ts = Left $ TrackIdAlreadyExists tid
  | otherwise = Right $ MusicLibrary $ Map.insert tid (mkTrackNode t) ts

mkTrackNodePred :: (Track -> Bool) -> (TrackNode -> Bool)
mkTrackNodePred p =
  \trackNode -> p trackNode.track

filterLib :: (Track -> Bool) -> MusicLibrary -> MusicLibrary
filterLib p (MusicLibrary ts) =
  MusicLibrary $ Map.filter (mkTrackNodePred p) ts

getTracks :: MusicLibrary -> [Track]
getTracks (MusicLibrary lib) = fmap (.track) $ Map.elems lib

getPairs :: MusicLibrary -> [(TrackId, Track)]
getPairs (MusicLibrary lib) =
  fmap (\(tid, node) -> (tid, node.track)) $ Map.toList lib

getTrackIds :: MusicLibrary -> [TrackId]
getTrackIds (MusicLibrary lib) = Map.keys lib

getTracksMatching :: (Track -> Bool) -> MusicLibrary -> [Track]
getTracksMatching p = getTracks . filterLib p

getTrackIdsMatching :: (Track -> Bool) -> MusicLibrary -> [TrackId]
getTrackIdsMatching p =
  getTrackIds . filterLib p

getTracksWithKey :: Key -> MusicLibrary -> [TrackId]
getTracksWithKey desiredKey =
  getTrackIdsMatching ((== desiredKey) . (.key))

sameKeySuccessors :: TrackId -> MusicLibrary -> [TrackId]
sameKeySuccessors trackId catalog =
  maybe [] (filter (/= trackId) . (`getTracksWithKey` catalog)) (trackKey trackId catalog)

keyJumpSuccessors :: TrackId -> MusicLibrary -> [TrackId]
keyJumpSuccessors trackId catalog =
  maybe [] (flip getTracksWithKeys catalog . compatibleKeys) (trackKey trackId catalog)

harmonicSuccessors :: TrackId -> MusicLibrary -> [TrackId]
harmonicSuccessors trackId catalog =
  sameKeySuccessors trackId catalog ++ keyJumpSuccessors trackId catalog

getTracksWithKeys :: [Key] -> MusicLibrary -> [TrackId]
getTracksWithKeys keys catalog =
  concatMap (`getTracksWithKey` catalog) keys

getTrackWithId :: TrackId -> MusicLibrary -> Maybe Track
getTrackWithId tid (MusicLibrary lib) =
  (.track) <$> Map.lookup tid lib

trackKey :: TrackId -> MusicLibrary -> Maybe Key
trackKey trackId catalog =
  fmap (.key) (getTrackWithId trackId catalog)

-- TODO: return Either?
getEdges ::
  TrackId ->
  MusicLibrary ->
  [(TrackId, TestedTransition)]
getEdges tid lib = do
  case getNode tid lib of
    Nothing -> []
    Just node -> Map.toList $ node.testedTransitions

emptyMusicLibrary :: MusicLibrary
emptyMusicLibrary = MusicLibrary $ Map.empty
