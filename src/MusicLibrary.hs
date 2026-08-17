module MusicLibrary
  ( MusicLibrary (..),
  -- empty,
  -- addTracks,
  -- addTransitions,
  )
where

import Track
import TrackCatalog (TrackCatalog, addTrack, emptyCatalog)
import TransitionsGraph (Edge, Graph, addEdge, emptyGraph)

----------------------------------
----- Data -----------------------
----------------------------------

data MusicLibrary = MusicLibrary
  { trackCatalog :: TrackCatalog,
    transitionGraph :: Graph,
    nextTrackId :: Int
  }
  deriving (Show)

----------------------------------
----- Functions ------------------
----------------------------------

-- TODO: reinstate those once the data structure is "final".
-- But until now it's ok to play with the raw dats structure.

empty :: MusicLibrary
empty =
  MusicLibrary
    { trackCatalog = emptyCatalog,
      transitionGraph = emptyGraph,
      nextTrackId = 0
    }

addTrack_ :: Track -> MusicLibrary -> MusicLibrary
addTrack_ track (MusicLibrary catalog graph nextId) =
  MusicLibrary
    { trackCatalog = addTrack nextId track catalog,
      transitionGraph = graph,
      nextTrackId = nextId + 1
    }

addTracks :: [Track] -> MusicLibrary -> MusicLibrary
addTracks [] lib = lib
addTracks (t : ts) lib = addTracks ts (addTrack_ t lib)

-- TODO: this should check that the track ID exists in the track catalog (and give error if not)
addTransition :: TrackId -> Edge -> MusicLibrary -> MusicLibrary
addTransition trackId edge (MusicLibrary catalog graph nextId) =
  MusicLibrary
    { trackCatalog = catalog,
      transitionGraph = addEdge trackId edge graph,
      nextTrackId = nextId
    }

addTransitions :: [(TrackId, Edge)] -> MusicLibrary -> MusicLibrary
addTransitions [] lib = lib
addTransitions ((tid, edge) : ts) lib =
  addTransitions ts $ addTransition tid edge lib
