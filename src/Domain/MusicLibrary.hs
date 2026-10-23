module Domain.MusicLibrary
  ( MusicLibrary,
    MusicLibraryError (..),
    emptyMusicLibrary,
    Track (..),
    TrackId,
    withTrack,
    getTrackIds,
    getTracksMatching,
    getTrackWithId,
    getTracks,
    keyJumpSuccessors,
    withTransition,
    sameKeySuccessors,
    titleMatches,
    keyMatches,
    TestedTransition (..),
    getEdges,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Domain.Camelot (Key, compatibleKeys)

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

type TrackId = String -- for now...

data Track = Track
  { title :: Text,
    key :: Key
  }
  deriving (Eq)

data TestedTransition = TestedTransition
  { notes :: Maybe String
  -- rating
  -- etc.
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

withNodeTransition :: TrackId -> TestedTransition -> TrackNode -> TrackNode
withNodeTransition dest tdata node =
  TrackNode
    node.track
    (Map.insert dest tdata node.testedTransitions)

withTransition ::
  TrackId ->
  TrackId ->
  TestedTransition ->
  MusicLibrary ->
  Either MusicLibraryError MusicLibrary
withTransition from to info lib
  | from == to = Left $ TransitionHasSameSourceAndDest from
  | otherwise = do
      fromNode <- requireNode from
      _ <- requireNode to
      if Map.member to fromNode.testedTransitions
        then Left $ TestedTransitionAlreadyExists from to
        else pure $ withNode from (withNodeTransition to info fromNode) lib
  where
    requireNode tid =
      maybe
        (Left $ TransitionHasInvalidTrackId tid)
        Right
        (getNode tid lib)

{---------------------------------------------------------------------------
  Functions
----------------------------------------------------------------------------}

instance Show Track where
  show (Track trackTitle trackKey') =
    "\"" ++ T.unpack trackTitle ++ "\" (" ++ show trackKey' ++ ")"

titleMatches :: Text -> Track -> Bool
titleMatches s t =
  T.isInfixOf (T.toLower s) (T.toLower t.title)

keyMatches :: Key -> Track -> Bool
keyMatches k t = k == t.key

withTrack ::
  (TrackId, Track) ->
  MusicLibrary ->
  Either MusicLibraryError MusicLibrary
withTrack (tid, t) (MusicLibrary ts)
  | Map.member tid ts = Left $ TrackIdAlreadyExists tid
  | otherwise = Right $ MusicLibrary $ Map.insert tid (mkTrackNode t) ts

mkTrackNodePred :: (Track -> Bool) -> (TrackNode -> Bool)
mkTrackNodePred p =
  \trackNode -> p trackNode.track

getTracksMatching :: (Track -> Bool) -> MusicLibrary -> [(TrackId, Track)]
getTracksMatching p (MusicLibrary ts) =
  fmap
    (\kv -> (fst kv, (.track) $ snd kv))
    (Map.toList $ Map.filter (mkTrackNodePred p) ts)

getTracks :: MusicLibrary -> [(TrackId, Track)]
getTracks = getTracksMatching (const True)

getTrackIds :: MusicLibrary -> [TrackId]
getTrackIds (MusicLibrary lib) = Map.keys lib

getTrackIdsMatching :: (Track -> Bool) -> MusicLibrary -> [TrackId]
getTrackIdsMatching p =
  fmap fst . getTracksMatching p

getTracksWithKey :: Key -> MusicLibrary -> [TrackId]
getTracksWithKey desiredKey =
  getTrackIdsMatching ((== desiredKey) . (.key))

sameKeySuccessors :: TrackId -> MusicLibrary -> [TrackId]
sameKeySuccessors trackId lib =
  maybe [] (filter (/= trackId) . (`getTracksWithKey` lib)) (trackKey trackId lib)

keyJumpSuccessors :: TrackId -> MusicLibrary -> [TrackId]
keyJumpSuccessors trackId lib =
  maybe [] (flip getTracksWithKeys lib . compatibleKeys) (trackKey trackId lib)

getTracksWithKeys :: [Key] -> MusicLibrary -> [TrackId]
getTracksWithKeys keys lib =
  concatMap (`getTracksWithKey` lib) keys

getTrackWithId :: TrackId -> MusicLibrary -> Maybe Track
getTrackWithId tid (MusicLibrary lib) =
  (.track) <$> Map.lookup tid lib

trackKey :: TrackId -> MusicLibrary -> Maybe Key
trackKey trackId lib =
  fmap (.key) (getTrackWithId trackId lib)

getEdges ::
  TrackId ->
  MusicLibrary ->
  [(TrackId, TestedTransition)]
getEdges tid lib =
  maybe
    []
    (Map.toList . (.testedTransitions))
    (getNode tid lib)

emptyMusicLibrary :: MusicLibrary
emptyMusicLibrary = MusicLibrary $ Map.empty
