{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lib
  ( Key (..),
    KeyNumber (..),
    UserRating (..),
    KeyType (..),
    compatibleKeys,
    TrackId,
    Track (..),
    TrackCatalog,
    sameKeySuccessors,
    keyJumpSuccessors,
    harmonicSuccessors,
    emptyCatalog,
    addTrack,
    getTrackWithId,
    Graph,
    Edge (..),
    confirmedSuccessors,
    emptyGraph,
    addEdge,
    MusicLibrary (..),
    Score,
    TransitionType (..),
    Transition (..),
    scoredNextTracks,
    printTrackCatalog,
  )
where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map

-----------------------------------------
----- Camelot wheel ---------------------
-----------------------------------------

data Key = Key KeyNumber KeyType deriving (Eq)

data KeyNumber
  = K_1
  | K_2
  | K_3
  | K_4
  | K_5
  | K_6
  | K_7
  | K_8
  | K_9
  | K_10
  | K_11
  | K_12
  deriving (Eq, Ord, Enum, Bounded)

data KeyType
  = D
  | M
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Show KeyNumber where
  show K_1 = "1"
  show K_2 = "2"
  show K_3 = "3"
  show K_4 = "4"
  show K_5 = "5"
  show K_6 = "6"
  show K_7 = "7"
  show K_8 = "8"
  show K_9 = "9"
  show K_10 = "10"
  show K_11 = "11"
  show K_12 = "12"

instance Show Key where
  show (Key n t) = show n ++ show t

compatibleKeys :: Key -> [Key]
compatibleKeys k =
  [increment k, decrement k, relative k]

incrementKeyNumber :: Int -> KeyNumber -> KeyNumber
incrementKeyNumber n k =
  toEnum ((fromEnum k + n) `mod` 12)

add :: Int -> Key -> Key
add n (Key number keyType) =
  Key (incrementKeyNumber n number) keyType

relative :: Key -> Key
relative (Key number D) = Key number M
relative (Key number M) = Key number D

increment, decrement :: Key -> Key
increment = add 1
decrement = add (-1)

-----------------------------------------
----- Tracks & Catalog  -----------------
-----------------------------------------

type TrackId = Int

data Track
  = Track
  { title :: String,
    key :: Key
  }
  deriving (Eq)

instance Show Track where
  show (Track trackTitle trackKey') =
    "\"" ++ trackTitle ++ "\" (" ++ show trackKey' ++ ")"

type TrackCatalog = Map.Map TrackId Track

emptyCatalog :: TrackCatalog
emptyCatalog = Map.empty

addTrack :: TrackId -> Track -> TrackCatalog -> TrackCatalog
addTrack = Map.insert

getTracks :: (Track -> Bool) -> TrackCatalog -> [TrackId]
getTracks predicate =
  Map.keys . Map.filter predicate

getTracksWithKey :: Key -> TrackCatalog -> [TrackId]
getTracksWithKey desiredKey =
  getTracks ((== desiredKey) . (.key))

sameKeySuccessors :: TrackId -> TrackCatalog -> [TrackId]
sameKeySuccessors trackId catalog =
  maybe [] (filter (/= trackId) . (`getTracksWithKey` catalog)) (trackKey trackId catalog)

keyJumpSuccessors :: TrackId -> TrackCatalog -> [TrackId]
keyJumpSuccessors trackId catalog =
  maybe [] (flip getTracksWithKeys catalog . compatibleKeys) (trackKey trackId catalog)

harmonicSuccessors :: TrackId -> TrackCatalog -> [TrackId]
harmonicSuccessors trackId catalog =
  sameKeySuccessors trackId catalog ++ keyJumpSuccessors trackId catalog

getTracksWithKeys :: [Key] -> TrackCatalog -> [TrackId]
getTracksWithKeys keys catalog =
  concatMap (`getTracksWithKey` catalog) keys

trackKey :: TrackId -> TrackCatalog -> Maybe Key
trackKey trackId catalog =
  (.key) <$> Map.lookup trackId catalog

getTrackWithId :: TrackId -> TrackCatalog -> Maybe Track
getTrackWithId = Map.lookup

-----------------------------------------
----- Confirmed transitions graph -------
-----------------------------------------

type Graph = Map.Map TrackId [Edge]

data UserRating
  = Omg
  | Nice
  deriving (Show, Eq, Ord)

data Edge = Edge
  { to :: TrackId,
    rating :: UserRating,
    notes :: Maybe String
  }
  deriving (Eq, Show)

emptyGraph :: Graph
emptyGraph = Map.empty

addEdge :: TrackId -> Edge -> Graph -> Graph
addEdge trackId edge =
  Map.insert trackId [edge]

getEdges :: TrackId -> Graph -> [Edge]
getEdges = Map.findWithDefault []

confirmedSuccessors :: TrackId -> Graph -> [TrackId]
confirmedSuccessors trackId = map (.to) . getEdges trackId

-----------------------------------------
----- Music Library ---------------------
-----------------------------------------

data MusicLibrary = MusicLibrary
  { trackCatalog :: TrackCatalog,
    transitionGraph :: Graph,
    nextTrackId :: Int
  }
  deriving (Show)

-----------------------------------------
----- Transitions -----------------------
-----------------------------------------

data TransitionType
  = Tested
  | SameKey
  | Harmonic
  deriving (Eq, Ord)

instance Show TransitionType where
  show Tested = "Tested"
  show SameKey = "Same key"
  show Harmonic = "Harmonic"

data Transition = Transition
  { transitionType :: TransitionType,
    to :: TrackId
  }
  deriving (Eq, Show)

type Score = Int

scoredNextTracks :: MusicLibrary -> TrackId -> [(Maybe Track, Score)]
scoredNextTracks musicLibrary trackId =
  map resolve (scoredTransitions musicLibrary trackId)
  where
    resolve (Transition _ toId, score) =
      (getTrackWithId toId (musicLibrary.trackCatalog), score)

scoredTransitions :: MusicLibrary -> TrackId -> [(Transition, Score)]
scoredTransitions musicLibrary =
  sortOn snd . scoreTransitions musicLibrary . transitions musicLibrary

transitions :: MusicLibrary -> TrackId -> [Transition]
transitions (MusicLibrary catalog graph _) trackId =
  sameKey ++ harmonic ++ tested
  where
    sameKey = makeTransitions SameKey (sameKeySuccessors trackId catalog)
    harmonic = makeTransitions Harmonic (keyJumpSuccessors trackId catalog)
    tested = makeTransitions Tested (confirmedSuccessors trackId graph)
    makeTransitions = map . Transition

scoreTransitions :: MusicLibrary -> [Transition] -> [(Transition, Score)]
scoreTransitions musicLibrary values =
  zip values (map (scoreTransition musicLibrary) values)

scoreTransition :: MusicLibrary -> Transition -> Score
scoreTransition _ (Transition Tested _) = 0
scoreTransition _ (Transition SameKey _) = 1
scoreTransition _ (Transition Harmonic _) = 2

-----------------------------------------
----- Pretty Print ----------------------
-----------------------------------------

printTrackCatalog :: TrackCatalog -> IO ()
printTrackCatalog =
  putStr . unlines . map formatTrack . Map.toList
  where
    formatTrack (trackId, track) =
      show trackId ++ ": " ++ show track
