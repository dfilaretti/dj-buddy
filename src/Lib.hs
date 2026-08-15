module Lib
  ( someFunc,
    module Library,
    scoreCandidates,
    Candidate (..),
    confirmedSuccessors,
    candidates,
    scoreCandidate,
    rankedCandidates,
    bestCandidates,
    -- , sameKeySuccessors
    TransitionType (..),
    Edge (..),
    Graph,
  )
where

import Data.List (groupBy, sortOn)
import qualified Data.Map.Strict as Map
import Library

someFunc :: IO ()
someFunc = putStrLn "someFunc"

----------------------------------
----- Graph ----------------------
----------------------------------

type Graph = Map.Map TrackId [Edge]

-- TODO: rename this to Transition
data Edge = Edge
  { to :: TrackId
  -- notes     :: String
  -- rating    :: Rating
  --
  }
  deriving (Eq, Show)

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

----------------------------------
----- Candidate ------------------
----------------------------------

data Candidate = Candidate
  { transitionType :: TransitionType,
    track :: TrackId
  }
  deriving (Eq, Show)

data TransitionType
  = Tested
  | SameKey
  | Harmonic
  deriving (Eq, Ord, Show)

mkCandidates :: TransitionType -> [TrackId] -> [Candidate]
mkCandidates t = map (Candidate t)

candidates :: Library -> Graph -> TrackId -> [Candidate]
candidates lib graph trackId =
  sameKey ++ harmonic ++ tested
  where
    sameKey = mkCandidates SameKey (sameKeySuccessors trackId lib)
    harmonic = mkCandidates Harmonic (keyJumpSuccessors trackId lib)
    tested = mkCandidates Tested (confirmedSuccessors trackId graph)

scoreCandidates :: Library -> Graph -> [Candidate] -> [(Candidate, Score)]
scoreCandidates lib graph xs =
  zip xs scores
  where
    scores = fmap (scoreCandidate lib graph) xs

rankedCandidates :: Library -> Graph -> TrackId -> [(Candidate, Score)]
rankedCandidates lib graph =
  sortOn snd . scoreCandidates lib graph . candidates lib graph

bestCandidates :: Library -> Graph -> TrackId -> [Candidate]
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
scoreCandidate :: Library -> Graph -> Candidate -> Score
scoreCandidate _ _ (Candidate Tested _) = 0
scoreCandidate _ _ (Candidate SameKey _) = 1
scoreCandidate _ _ (Candidate Harmonic _) = 2
