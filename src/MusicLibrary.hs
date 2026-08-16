module MusicLibrary
  ( MusicLibrary,
    empty,
    addTracks,
    addTransitions,
  )
where

import qualified Graph as G
import qualified Track as Track
import qualified TrackCatalog as T

data MusicLibrary = MusicLibrary
  { trackCatalog :: T.TrackCatalog,
    transitionGraph :: G.Graph,
    nextTrackId :: Int
  }
  deriving (Show)

empty :: MusicLibrary
empty =
  MusicLibrary
    { trackCatalog = T.empty,
      transitionGraph = G.empty,
      nextTrackId = 0
    }

addTrack :: Track.Track -> MusicLibrary -> MusicLibrary
addTrack track (MusicLibrary catalog graph nextId) =
  MusicLibrary
    { trackCatalog = T.add nextId track catalog,
      transitionGraph = graph,
      nextTrackId = nextId + 1
    }

addTracks :: [Track.Track] -> MusicLibrary -> MusicLibrary
addTracks [] lib = lib
addTracks (t : ts) lib = addTracks ts (addTrack t lib)

-- TODO: this should check that the track ID exists in the track catalog (and give error if not)
addTransition :: Track.TrackId -> G.Edge -> MusicLibrary -> MusicLibrary
addTransition trackId edge (MusicLibrary catalog graph nextId) =
  MusicLibrary
    { trackCatalog = catalog,
      transitionGraph = G.addEdge trackId edge graph,
      nextTrackId = nextId
    }

addTransitions :: [(Track.TrackId, G.Edge)] -> MusicLibrary -> MusicLibrary
addTransitions [] lib = lib
addTransitions ((tid, edge) : ts) lib =
  addTransitions ts $ addTransition tid edge lib
