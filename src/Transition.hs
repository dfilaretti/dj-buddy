module Transition
  ( Score,
    TransitionType (..),
    Transition (..),
    bestTransitions,
    sortedTransitions,
  )
where

import Data.List (groupBy, sortOn)
import Graph (Graph, confirmedSuccessors)
import Track (TrackId)
import TrackCatalog (TrackCatalog, keyJumpSuccessors, sameKeySuccessors)

----------------------------------
----- Data -----------------------
----------------------------------

data TransitionType
  = Tested
  | SameKey
  | Harmonic
  deriving (Eq, Ord, Show)

data Transition = Transition
  { transitionType :: TransitionType,
    track :: TrackId
  }
  deriving (Eq, Show)

type Score = Int

----------------------------------
----- Public functions -----------
----------------------------------

sortedTransitions :: TrackCatalog -> Graph -> TrackId -> [(Transition, Score)]
sortedTransitions lib graph =
  sortOn snd . scoreTransitions lib graph . transitions lib graph

bestTransitions :: TrackCatalog -> Graph -> TrackId -> [Transition]
bestTransitions lib graph x =
  case sortedTransitions lib graph x of
    [] -> []
    xs -> fmap fst $ head $ groupBy p $ xs
  where
    p = \c1 c2 -> snd c1 == snd c2

----------------------------------
----- Internal functions ---------
----------------------------------

mkTransitions :: TransitionType -> [TrackId] -> [Transition]
mkTransitions t = map (Transition t)

transitions :: TrackCatalog -> Graph -> TrackId -> [Transition]
transitions lib graph trackId =
  sameKey ++ harmonic ++ tested
  where
    sameKey = mkTransitions SameKey (sameKeySuccessors trackId lib)
    harmonic = mkTransitions Harmonic (keyJumpSuccessors trackId lib)
    tested = mkTransitions Tested (confirmedSuccessors trackId graph)

scoreTransitions :: TrackCatalog -> Graph -> [Transition] -> [(Transition, Score)]
scoreTransitions lib graph xs =
  zip xs scores
  where
    scores = fmap (scoreTransition lib graph) xs

-- this is where the magic will eventually happen...
scoreTransition :: TrackCatalog -> Graph -> Transition -> Score
scoreTransition _ _ (Transition Tested _) = 0
scoreTransition _ _ (Transition SameKey _) = 1
scoreTransition _ _ (Transition Harmonic _) = 2
