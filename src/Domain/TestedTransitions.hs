module Domain.TestedTransitions
  ( TestedTransition (..),
    TestedTransitionsGraph,
    emptyGraph,
    getEdges,
    addEdge,
  )
where

import Data.Map.Strict qualified as Map
import Domain.TrackCatalog (TrackId)

type TestedTransitionsGraph = Map.Map TrackId [TestedTransition]

data UserRating
  = Nice
  | Omg
  deriving (Show, Eq, Ord)

data TestedTransition = TestedTransition
  { to :: TrackId,
    notes :: Maybe String
  }
  deriving (Show, Eq)

emptyGraph :: TestedTransitionsGraph
emptyGraph = Map.empty

transExists ::
  TestedTransitionsGraph ->
  TrackId -> -- from
  TrackId -> -- to
  Bool
transExists g from to =
  any
    (\tt -> tt.to == to)
    (getEdges from g)

addEdge ::
  TrackId ->
  TestedTransition ->
  TestedTransitionsGraph ->
  TestedTransitionsGraph
addEdge tid t g
  | transExists g tid t.to = error "WOOOOOO"
  | otherwise =
      Map.alter f tid g
  where
    f x = case x of
      Nothing -> Just [t]
      Just ts -> Just (t : ts)

getEdges ::
  TrackId ->
  TestedTransitionsGraph ->
  [TestedTransition]
getEdges = Map.findWithDefault []

{-
 [] Add Error type for this module
 [] addEdge will then return Either ErrorType TestedTransitionsGraph
-}
