module Lib
    ( someFunc
    , TrackId
    , Track(..)
    , Library
    , emptyLibrary
    , addToLibrary
    , filterLibrary
    , getTrackIds
    -- , sameKeySuccessors
    , Transition(..)
    , Edge(..)
    , Graph
    , trackKey
    ) where

import qualified Data.Map.Strict as Map
import qualified Key as Key

someFunc :: IO ()
someFunc = putStrLn "someFunc"

----------------------------------
----- Other ----------------------
----------------------------------

type TrackId = Int

data Track = Track
    { title :: String
    , key :: Key.Key
    }
    deriving (Eq, Show)

type Library = Map.Map TrackId Track

emptyLibrary :: Library
emptyLibrary = Map.fromList []

addToLibrary :: TrackId -> Track -> Library -> Library
addToLibrary trackId track lib =
    Map.insert trackId track lib

filterLibrary :: (Track -> Bool) -> Library -> Library
filterLibrary p = Map.filter p

getTrackIds :: (Track -> Bool) -> Library -> [TrackId]
getTrackIds p  = Map.keys . filterLibrary p

data Transition
    = Tested
    | SameKey
    | Harmonic
    deriving (Eq, Ord, Show)

data Edge = Edge
    { to         :: TrackId
    , transition :: Transition
    -- TODO: add notes etc.
    }
    deriving (Eq, Show)

type Graph = Map.Map TrackId [Edge]

----------------------------------
----- Functions ------------------
----------------------------------

trackKey :: TrackId -> Library -> Maybe Key.Key
trackKey n lib = key <$> Map.lookup n lib

-- sameKeySuccessors :: Library -> TrackId -> [TrackId]
--sameKeySuccessors lib tId = Map.delete tId filtered
--    where
--        desiredKey = trackKey tId lib
--        filtered = case desiredKey of
--            Just k -> Map.filter pred lib
--                where pred = \t -> key t == k
--            Nothing -> emptyLibrary





-- title <$> Map.lookup (TrackId 1) library
-- [(tid, title track) | (tid, track) <- Map.toList library]
-- [edge | edge <- graph Map.! TrackId 1]
