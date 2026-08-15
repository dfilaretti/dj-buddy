module Lib
    ( someFunc
    , TrackId
    , Track(..)
    , Library
    , emptyLibrary
    , addToLibrary
    , removeFromLibrary
    , filterLibrary
    , getTracks
    , sameKeySuccessors
    , keyJumpSuccessors
    , harmonicSuccessors
    , getTracksWithKey
    , getTracksWithKeys
    , confirmedSuccessors
    , candidates
    , scoreCandidate
    , rankedCandidates
    , highestScoringCandidates
    -- , sameKeySuccessors
    , TransitionType(..)
    , Edge(..)
    , Graph
    , trackKey
    ) where

import qualified Data.Map.Strict as Map
import qualified Key as Key
import Data.List(sortOn, groupBy)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

----------------------------------
----- Track ----------------------
----------------------------------

type TrackId = Int

data Track = Track
    { title :: String
    , key :: Key.Key
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
getTracks p  = Map.keys . filterLibrary p

getTracksWithKey :: Key.Key -> Library -> [TrackId]
getTracksWithKey k = getTracks ((==k) . key)

-- all the tracks with the same key as this one
sameKeySuccessors :: TrackId -> Library -> [TrackId]
sameKeySuccessors tId lib =
    case desiredKey of
        Just k  -> filter (/= tId) $ getTracksWithKey k lib
        Nothing -> []
    where
        desiredKey = trackKey tId lib

-- all the tracks we can jump to based on the Camelot wheel, excluding same key!
keyJumpSuccessors :: TrackId -> Library -> [TrackId]
keyJumpSuccessors tId lib =
    case initialKey of
        Just k  -> getTracksWithKeys (Key.compatibleKeys k) lib
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
    { to         :: TrackId
    -- notes     :: String
    -- rating    :: Rating
    --
    }
    deriving (Eq, Show)

getEdges :: TrackId -> Graph -> Maybe [Edge]
getEdges tId g = Map.lookup tId g

-- all the tracks that are confirmed (by the user) to be good transition from this one
-- TODO: this should return [Edge]
confirmedSuccessors :: TrackId -> Graph -> [TrackId]
confirmedSuccessors tId g =
    case getEdges tId g of
        Nothing    -> []
        Just edges -> map to edges

----------------------------------
----- Candidate ------------------
----------------------------------

data Candidate = Candidate
    { transitionType :: TransitionType
    , track          :: TrackId
    }
    deriving (Eq, Show)

data TransitionType
    = Tested
    | SameKey
    | Harmonic
    deriving (Eq, Ord, Show)

mkCandidates :: TransitionType -> [TrackId] -> [Candidate]
mkCandidates transitionType = map (Candidate transitionType)

candidates :: Library -> Graph -> TrackId -> [Candidate]
candidates lib graph trackId =
    sameKeyCandidates ++ harmonicCandidates ++ testedCandidates
    where
        sameKeyCandidates  = mkCandidates SameKey (sameKeySuccessors trackId lib)
        harmonicCandidates = mkCandidates Harmonic (keyJumpSuccessors trackId lib)
        testedCandidates   = mkCandidates Tested (confirmedSuccessors trackId graph)

rankedCandidates :: Library -> Graph -> TrackId -> [(Candidate, Score)]
rankedCandidates lib graph trackId =
    sortOn snd $ zip cand scores
        where
            cand   = candidates lib graph trackId
            scores = fmap scoreCandidate cand

highestScoringCandidates :: Library -> Graph -> TrackId -> [Candidate]
highestScoringCandidates lib graph x  =
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
scoreCandidate :: Candidate -> Int
scoreCandidate (Candidate Tested _)   = 0
scoreCandidate (Candidate SameKey _)  = 1
scoreCandidate (Candidate Harmonic _) = 2
