module Domain.MusicLibrary (MusicLibrary (..), MusicLibraryError, insertTransition, isValidTransition, emptyMusicLibrary, getTrack) where

import Domain.TestedTransitions
import Domain.TrackCatalog

data MusicLibrary = MusicLibrary
  { trackCatalog :: TrackCatalog,
    transitionGraph :: TestedTransitionsGraph
  }
  deriving (Show)

data MusicLibraryError
  = InsertingMalformedTransition TrackId TestedTransition
  deriving (Show, Eq)

emptyMusicLibrary :: MusicLibrary
emptyMusicLibrary =
  MusicLibrary
    { trackCatalog = emptyCatalog,
      transitionGraph = emptyGraph
    }

isValidTransition ::
  (TrackId, TestedTransition) ->
  MusicLibrary ->
  Bool
isValidTransition (tid, t) lib =
  and
    [ tid /= t.to,
      tid `elem` validIds,
      t.to `elem` validIds
    ]
  where
    validIds = getAllTrackIds lib.trackCatalog

insertTransition_ ::
  TrackId ->
  TestedTransition ->
  MusicLibrary ->
  MusicLibrary
insertTransition_ tid t lib =
  MusicLibrary {trackCatalog = c', transitionGraph = g'}
  where
    c' = lib.trackCatalog
    g' = addEdge tid t lib.transitionGraph

insertTransition ::
  TrackId ->
  TestedTransition ->
  MusicLibrary ->
  Either MusicLibraryError MusicLibrary
insertTransition tid t lib
  | isValidTransition (tid, t) lib = Right (insertTransition_ tid t lib)
  | otherwise = Left (InsertingMalformedTransition tid t)

getTrack :: TrackId -> MusicLibrary -> Maybe Track
getTrack tid lib = getTrackWithId tid lib.trackCatalog
