module NextTransition
  ( Score,
    TransitionType (..),
    Transition (..),
    bestTransitions,
    sortedTransitions,
  )
where

import Data.List (groupBy, sortOn)
import MusicLibrary (MusicLibrary (..))
import Track (TrackId)
import TrackCatalog (keyJumpSuccessors, sameKeySuccessors)
import TransitionsGraph (confirmedSuccessors)

----------------------------------
----- Data -----------------------
----------------------------------

data TransitionType
  = Tested
  | SameKey
  | Harmonic
  deriving (Eq, Ord)

instance Show TransitionType where
  show Tested = "Tested"
  show SameKey = "Same key"
  show Harmonic = "Harmonic"

-- TODO: this should have both to/from tracks!
data Transition = Transition
  { transitionType :: TransitionType,
    to :: TrackId
  }
  deriving (Eq, Show)

type Score = Int

----------------------------------
----- Public functions -----------
----------------------------------

sortedTransitions :: MusicLibrary -> TrackId -> [(Transition, Score)]
sortedTransitions musicLib =
  sortOn snd . scoreTransitions musicLib . transitions musicLib

bestTransitions :: MusicLibrary -> TrackId -> [Transition]
bestTransitions ml x =
  case sortedTransitions ml x of
    [] -> []
    xs -> fmap fst $ head $ groupBy p $ xs
  where
    p = \c1 c2 -> snd c1 == snd c2

----------------------------------
----- Internal functions ---------
----------------------------------

transitions :: MusicLibrary -> TrackId -> [Transition]
transitions (MusicLibrary lib graph _) trackId =
  sameKey ++ harmonic ++ tested
  where
    sameKey = mkTransitions SameKey (sameKeySuccessors trackId lib)
    harmonic = mkTransitions Harmonic (keyJumpSuccessors trackId lib)
    tested = mkTransitions Tested (confirmedSuccessors trackId graph)
    mkTransitions = map . Transition

scoreTransitions :: MusicLibrary -> [Transition] -> [(Transition, Score)]
scoreTransitions ml xs =
  zip xs scores
  where
    scores = fmap (scoreTransition ml) xs

-- this is where the magic will eventually happen...
scoreTransition :: MusicLibrary -> Transition -> Score
scoreTransition _ (Transition Tested _) = 0
scoreTransition _ (Transition SameKey _) = 1
scoreTransition _ (Transition Harmonic _) = 2
