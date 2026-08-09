module Lib
    ( someFunc
    , Key(..)
    , TrackId(..)
    , Track(..)
    , Library
    , Transition(..)
    , Edge(..)
    , Graph
    ) where

import qualified Data.Map.Strict as Map

someFunc :: IO ()
someFunc = putStrLn "someFunc"

----------------------------------
----- Data -----------------------
----------------------------------

-- TODO: do I need Ord here?
-- TODO: instead of Int, can we be more specifc and use an enum or something?
data Key
    = A Int
    | B Int
    deriving (Eq, Ord, Show)

newtype TrackId = TrackId Int
    deriving (Eq, Ord, Show)

data Track = Track
    { title :: String
    , key :: Key
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


----------------------------------
----- Notes ----------------------
----------------------------------

-- title <$> Map.lookup (TrackId 1) library
-- [(tid, title track) | (tid, track) <- Map.toList library]
-- [edge | edge <- graph Map.! TrackId 1]
