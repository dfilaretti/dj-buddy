module Graph
  ( Graph,
    Edge (..),
    confirmedSuccessors,
    empty,
    addEdge,
  )
where

import qualified Data.IntSet as Map.Map
import qualified Data.Map.Strict as Map
import Track (TrackId)

type Graph = Map.Map TrackId [Edge]

-- TODO: rename this to Transition
data Edge = Edge
  { to :: TrackId
  -- notes     :: String
  -- rating    :: Rating
  --
  }
  deriving (Eq, Show)

empty :: Graph
empty = Map.fromList []

addEdge :: TrackId -> Edge -> Graph -> Graph
addEdge trackId edge = Map.insert trackId [edge]

getEdges :: TrackId -> Graph -> [Edge]
getEdges tId g =
  case Map.lookup tId g of
    Nothing -> []
    Just edges -> edges

-- all the tracks that are confirmed (by the user) to be good transition from this one
-- TODO: maybe return a new type [ConfirmedSuccessor] so we can keep additional info?
confirmedSuccessors :: TrackId -> Graph -> [TrackId]
confirmedSuccessors tId g =
  map
    to
    (getEdges tId g)
