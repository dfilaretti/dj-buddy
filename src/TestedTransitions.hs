module TestedTransitions
  ( TestedTransition (..),
    TestedTransitionsGraph,
    emptyGraph,
    getEdges,
    addEdge,
  )
where

import Data.Map.Strict qualified as Map
import TrackCatalog (TrackId)

type TestedTransitionsGraph = Map.Map TrackId [TestedTransition]

data UserRating
  = Nice
  | Omg
  deriving (Show, Eq, Ord)

data TestedTransition = TestedTransition
  { to :: TrackId,
    notes :: Maybe String
  }
  deriving (Show)

-- TODO: is this "correct"? Should it obey certain laws?
instance Eq TestedTransition where
  t1 == t2 = t1.to == t2.to

emptyGraph :: TestedTransitionsGraph
emptyGraph = Map.empty

-- TODO: can generalize this and move into a utils file maybe?
safecons ::
  TestedTransition ->
  [TestedTransition] ->
  [TestedTransition]
safecons x xs =
  if x `elem` xs
    then
      error $ "Transition " ++ show x ++ " already exists!"
    else
      x : xs

addEdge ::
  TrackId ->
  TestedTransition ->
  TestedTransitionsGraph ->
  TestedTransitionsGraph
addEdge tid t g =
  Map.alter f tid g
  where
    f x = case x of
      Nothing -> Just [t]
      Just ts -> Just (safecons t ts)

getEdges ::
  TrackId ->
  TestedTransitionsGraph ->
  [TestedTransition]
getEdges = Map.findWithDefault []

-- getEdge :: TrackId -> TrackId -> Graph -> Maybe TestedTransition
-- getEdge tFrom tTo g =
--  case filter (\e -> e.to == tTo) (getEdges tFrom g) of
--    [] -> Nothing
--    [x] -> Just x
--    _ -> error "multiple edges with same destination - why?"

-- confirmedSuccessors :: TrackId -> Graph -> [TrackId]
-- confirmedSuccessors trackId = map (.to) . getEdges trackId
