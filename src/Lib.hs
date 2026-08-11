module Lib
    ( someFunc
    , TrackId
    , Track(..)
    , Library
    , emptyLibrary
    , addToLibrary
    , removeFromLibrary
    , filterLibrary
    , getTrackIds
    , sameKeySuccessors
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
addToLibrary = Map.insert

removeFromLibrary :: TrackId -> Library -> Library
removeFromLibrary = Map.delete

filterLibrary :: (Track -> Bool) -> Library -> Library
filterLibrary = Map.filter

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

sameKeySuccessors :: Library -> TrackId -> [TrackId]
sameKeySuccessors lib tId =
    case desiredKey of
        Just k  -> filter (/= tId) $ getTrackIds (\t -> key t == k) lib
        Nothing -> []
    where
        desiredKey = trackKey tId lib




-- title <$> Map.lookup (TrackId 1) library
-- [(tid, title track) | (tid, track) <- Map.toList library]
-- [edge | edge <- graph Map.! TrackId 1]
