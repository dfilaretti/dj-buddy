module Lib
    ( someFunc
    , TrackId(..)
    , Track(..)
    , Library
    , Transition(..)
    , Edge(..)
    , Graph
    ) where

import qualified Data.Map.Strict as Map
import qualified Key as Key

someFunc :: IO ()
someFunc = putStrLn "someFunc"

----------------------------------
----- Other ----------------------
----------------------------------

newtype TrackId = TrackId Int
    deriving (Eq, Ord, Show)

data Track = Track
    { title :: String
    , key :: Key.Key
    }
    deriving (Eq, Show)

type Library = Map.Map TrackId Track

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


-- title <$> Map.lookup (TrackId 1) library
-- [(tid, title track) | (tid, track) <- Map.toList library]
-- [edge | edge <- graph Map.! TrackId 1]
