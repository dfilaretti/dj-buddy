module MusicLibrary
  ( MusicLibrary (..),
    empty,
    addTracks,
    addTransitions,
    module Track,
  )
where

import Track
import qualified TrackCatalog as T
import qualified TransitionsGraph as TG

data MusicLibrary = MusicLibrary
  { trackCatalog :: T.TrackCatalog,
    transitionGraph :: TG.Graph,
    nextTrackId :: Int
  }
  deriving (Show)

empty :: MusicLibrary
empty =
  MusicLibrary
    { trackCatalog = T.empty,
      transitionGraph = TG.empty,
      nextTrackId = 0
    }

addTrack :: Track -> MusicLibrary -> MusicLibrary
addTrack track (MusicLibrary catalog graph nextId) =
  MusicLibrary
    { trackCatalog = T.add nextId track catalog,
      transitionGraph = graph,
      nextTrackId = nextId + 1
    }

addTracks :: [Track] -> MusicLibrary -> MusicLibrary
addTracks [] lib = lib
addTracks (t : ts) lib = addTracks ts (addTrack t lib)

-- TODO: this should check that the track ID exists in the track catalog (and give error if not)
addTransition :: TrackId -> TG.Edge -> MusicLibrary -> MusicLibrary
addTransition trackId edge (MusicLibrary catalog graph nextId) =
  MusicLibrary
    { trackCatalog = catalog,
      transitionGraph = TG.addEdge trackId edge graph,
      nextTrackId = nextId
    }

addTransitions :: [(TrackId, TG.Edge)] -> MusicLibrary -> MusicLibrary
addTransitions [] lib = lib
addTransitions ((tid, edge) : ts) lib =
  addTransitions ts $ addTransition tid edge lib
