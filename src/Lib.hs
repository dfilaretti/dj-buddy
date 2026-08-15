module Lib
  ( someFunc,
    TrackId,
    Track (..),
    Library,
    emptyLibrary,
    addToLibrary,
    removeFromLibrary,
    scoreCandidates,
    filterLibrary,
    getTracks,
    sameKeySuccessors,
    keyJumpSuccessors,
    harmonicSuccessors,
    getTracksWithKey,
    getTracksWithKeys,
    confirmedSuccessors,
    candidates,
    scoreCandidate,
    rankedCandidates,
    bestCandidates,
    -- , sameKeySuccessors
    TransitionType (..),
    Edge (..),
    Graph,
    trackKey,
  )
where

import Data.List (groupBy, sortOn)
import qualified Data.Map.Strict as Map
import qualified Key as Key

someFunc :: IO ()
someFunc = putStrLn "someFunc"

----------------------------------
----- Track ----------------------
----------------------------------

type TrackId = Int

data Track = Track
  { title :: String,
    key :: Key.Key
    -- , author: String
    -- etc.
  }
  deriving (Eq, Show)

----------------------------------
----- Library --------------------
----------------------------------

type Library = Map.Map TrackId Track

emptyLibrary :: Library
emptyLibrary = Map.fromList []

addToLibrary :: TrackId -> Track -> Library -> Library
addToLibrary = Map.insert

removeFromLibrary :: TrackId -> Library -> Library
removeFromLibrary = Map.delete

filterLibrary :: (Track -> Bool) -> Library -> Library
filterLibrary = Map.filter

getTracks :: (Track -> Bool) -> Library -> [TrackId]
getTracks p = Map.keys . filterLibrary p

getTracksWithKey :: Key.Key -> Library -> [TrackId]
getTracksWithKey k = getTracks ((== k) . key)

-- all the tracks with the same key as this one
sameKeySuccessors :: TrackId -> Library -> [TrackId]
sameKeySuccessors tId lib =
  case desiredKey of
    Just k -> filter (/= tId) $ getTracksWithKey k lib
    Nothing -> []
  where
    desiredKey = trackKey tId lib

-- all the tracks we can jump to based on the Camelot wheel, excluding same key!
keyJumpSuccessors :: TrackId -> Library -> [TrackId]
keyJumpSuccessors tId lib =
  case initialKey of
    Just k -> getTracksWithKeys (Key.compatibleKeys k) lib
    Nothing -> []
  where
    initialKey = trackKey tId lib

-- all the tracks we can jump to according to the Camelot wheel
-- TODO: this doesn't include weird jumps such as the "Armin Van Buuren transition" etc. Consider adding later.
harmonicSuccessors :: TrackId -> Library -> [TrackId]
harmonicSuccessors tId lib =
  sameKeySuccessors tId lib ++ keyJumpSuccessors tId lib

-- TODO: refactor?
getTracksWithKeys :: [Key.Key] -> Library -> [TrackId]
getTracksWithKeys ks lib =
  concat $ fmap (\k -> getTracksWithKey k lib) ks

trackKey :: TrackId -> Library -> Maybe Key.Key
trackKey n lib = key <$> Map.lookup n lib

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
rankedCandidates lib graph trackId =
  sortOn snd $ scoreCandidates lib graph $ candidates lib graph trackId

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
