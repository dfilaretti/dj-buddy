module NextTransition
  ( Score,
    TransitionType (..),
    Transition (..),
    scoredNextTracks,
  )
where

import Data.List (groupBy, sortOn)
import MusicLibrary (MusicLibrary (..))
import Track (Track, TrackId)
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

scoredNextTracks :: MusicLibrary -> TrackId -> [(Track, Score)]
scoredNextTracks = undefined

----------------------------------
----- Internal functions ---------
----------------------------------

scoredTransitions :: MusicLibrary -> TrackId -> [(Transition, Score)]
scoredTransitions musicLib =
  sortOn snd . scoreTransitions musicLib . transitions musicLib

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
