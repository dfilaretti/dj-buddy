module Domain.MusicLibrary (MusicLibrary (..), insertTransition, isValidTransition, emptyMusicLibrary, getTrack) where

import Domain.TestedTransitions
import Domain.TrackCatalog

data MusicLibrary = MusicLibrary
  { trackCatalog :: TrackCatalog,
    transitionGraph :: TestedTransitionsGraph
  }
  deriving (Show)

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
  MusicLibrary
insertTransition tid t lib =
  if isValidTransition (tid, t) lib
    then
      insertTransition_ tid t lib
    else
      error $ "insertTransition: track IDs '" ++ tid ++ "' and '" ++ t.to ++ "' must exist in track catalog, and can't be the same!"

getTrack :: TrackId -> MusicLibrary -> Maybe Track
getTrack tid lib = getTrackWithId tid lib.trackCatalog
