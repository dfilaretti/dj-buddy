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

getTracks :: (Track -> Bool) -> Library -> [TrackId]
getTracks p  = Map.keys . filterLibrary p

getTracksWithKey :: Key.Key -> Library -> [TrackId]
getTracksWithKey k = getTracks ((==k) . key)

-- TODO: refactor?
getTracksWithKeys :: [Key.Key] -> Library -> [TrackId]
getTracksWithKeys ks lib =
    concat $ fmap (\k -> getTracksWithKey k lib) ks

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

-- all the tracks that are confirmed (by the user) to be good transition from this one
confirmedSuccessors :: TrackId -> Graph -> [TrackId]
--confirmedSuccessort = _

-- title <$> Map.lookup (TrackId 1) library
-- [(tid, title track) | (tid, track) <- Map.toList library]
-- [edge | edge <- graph Map.! TrackId 1]
