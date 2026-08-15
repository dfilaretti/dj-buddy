module Lib
  ( someFunc,
    module Library,
    module Graph,
    scoreCandidates,
    Transition (..),
    candidates,
    scoreCandidate,
    rankedCandidates,
    bestCandidates,
    -- , sameKeySuccessors
    TransitionType (..),
  )
where

import Data.List (groupBy, sortOn)
import Graph
import Library

----------------------------------
----- Main  ----------------------
----------------------------------

someFunc :: IO ()
someFunc = putStrLn "someFunc"

----------------------------------
----- Transitions ----------------
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

mkCandidates :: TransitionType -> [TrackId] -> [Transition]
mkCandidates t = map (Transition t)

candidates :: Library -> Graph -> TrackId -> [Transition]
candidates lib graph trackId =
  sameKey ++ harmonic ++ tested
  where
    sameKey = mkCandidates SameKey (sameKeySuccessors trackId lib)
    harmonic = mkCandidates Harmonic (keyJumpSuccessors trackId lib)
    tested = mkCandidates Tested (confirmedSuccessors trackId graph)

scoreCandidates :: Library -> Graph -> [Transition] -> [(Transition, Score)]
scoreCandidates lib graph xs =
  zip xs scores
  where
    scores = fmap (scoreCandidate lib graph) xs

rankedCandidates :: Library -> Graph -> TrackId -> [(Transition, Score)]
rankedCandidates lib graph =
  sortOn snd . scoreCandidates lib graph . candidates lib graph

bestCandidates :: Library -> Graph -> TrackId -> [Transition]
bestCandidates lib graph x =
  case rankedCandidates lib graph x of
    [] -> []
    xs -> fmap fst $ head $ groupBy p $ xs
  where
    p = \c1 c2 -> snd c1 == snd c2

----------------------------------
----- Scoring --------------------
----------------------------------

type Score = Int

-- this is where the magic will eventually happen...
scoreCandidate :: Library -> Graph -> Transition -> Score
scoreCandidate _ _ (Transition Tested _) = 0
scoreCandidate _ _ (Transition SameKey _) = 1
scoreCandidate _ _ (Transition Harmonic _) = 2
