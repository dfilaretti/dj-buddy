{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lib
  ( Key (..),
    KeyNumber (..),
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
    library,
  )
where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map

-- Camelot keys
data Key = Key KeyNumber KeyType deriving (Eq)

data KeyNumber = K_1 | K_2 | K_3 | K_4 | K_5 | K_6 | K_7 | K_8 | K_9 | K_10 | K_11 | K_12
  deriving (Eq, Ord, Enum, Bounded)

data KeyType = D | M deriving (Eq, Ord, Enum, Bounded, Show)

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
compatibleKeys k = [increment k, decrement k, relative k]

incrementKeyNumber :: Int -> KeyNumber -> KeyNumber
incrementKeyNumber n k = toEnum ((fromEnum k + n) `mod` 12)

add :: Int -> Key -> Key
add n (Key number keyType) = Key (incrementKeyNumber n number) keyType

relative :: Key -> Key
relative (Key number D) = Key number M
relative (Key number M) = Key number D

increment, decrement :: Key -> Key
increment = add 1
decrement = add (-1)

-- Tracks and catalog
type TrackId = Int

data Track = Track {title :: String, key :: Key} deriving (Eq)

instance Show Track where
  show (Track trackTitle trackKey') = "\"" ++ trackTitle ++ "\" (" ++ show trackKey' ++ ")"

type TrackCatalog = Map.Map TrackId Track

emptyCatalog :: TrackCatalog
emptyCatalog = Map.empty

addTrack :: TrackId -> Track -> TrackCatalog -> TrackCatalog
addTrack = Map.insert

getTracks :: (Track -> Bool) -> TrackCatalog -> [TrackId]
getTracks predicate = Map.keys . Map.filter predicate

getTracksWithKey :: Key -> TrackCatalog -> [TrackId]
getTracksWithKey desiredKey = getTracks ((== desiredKey) . key)

sameKeySuccessors :: TrackId -> TrackCatalog -> [TrackId]
sameKeySuccessors trackId catalog =
  maybe [] (filter (/= trackId) . (`getTracksWithKey` catalog)) (trackKey trackId catalog)

keyJumpSuccessors :: TrackId -> TrackCatalog -> [TrackId]
keyJumpSuccessors trackId catalog =
  maybe [] (flip getTracksWithKeys catalog . compatibleKeys) (trackKey trackId catalog)

harmonicSuccessors :: TrackId -> TrackCatalog -> [TrackId]
harmonicSuccessors trackId catalog = sameKeySuccessors trackId catalog ++ keyJumpSuccessors trackId catalog

getTracksWithKeys :: [Key] -> TrackCatalog -> [TrackId]
getTracksWithKeys keys catalog = concatMap (`getTracksWithKey` catalog) keys

trackKey :: TrackId -> TrackCatalog -> Maybe Key
trackKey trackId catalog = key <$> Map.lookup trackId catalog

getTrackWithId :: TrackId -> TrackCatalog -> Maybe Track
getTrackWithId = Map.lookup

-- Confirmed transitions
type Graph = Map.Map TrackId [Edge]

data Edge = Edge {edgeTo :: TrackId} deriving (Eq, Show)

emptyGraph :: Graph
emptyGraph = Map.empty

addEdge :: TrackId -> Edge -> Graph -> Graph
addEdge trackId edge = Map.insert trackId [edge]

confirmedSuccessors :: TrackId -> Graph -> [TrackId]
confirmedSuccessors trackId graph = map edgeTo (Map.findWithDefault [] trackId graph)

-- Complete library
data MusicLibrary = MusicLibrary
  { trackCatalog :: TrackCatalog,
    transitionGraph :: Graph,
    nextTrackId :: Int
  }
  deriving (Show)

data TransitionType = Tested | SameKey | Harmonic deriving (Eq, Ord)

instance Show TransitionType where
  show Tested = "Tested"; show SameKey = "Same key"; show Harmonic = "Harmonic"

data Transition = Transition {transitionType :: TransitionType, to :: TrackId}
  deriving (Eq, Show)

type Score = Int

scoredNextTracks :: MusicLibrary -> TrackId -> [(Maybe Track, Score)]
scoredNextTracks musicLibrary trackId = map resolve (scoredTransitions musicLibrary trackId)
  where
    resolve (Transition _ toId, score) = (getTrackWithId toId (trackCatalog musicLibrary), score)

scoredTransitions :: MusicLibrary -> TrackId -> [(Transition, Score)]
scoredTransitions musicLibrary = sortOn snd . scoreTransitions musicLibrary . transitions musicLibrary

transitions :: MusicLibrary -> TrackId -> [Transition]
transitions (MusicLibrary catalog graph _) trackId = sameKey ++ harmonic ++ tested
  where
    sameKey = makeTransitions SameKey (sameKeySuccessors trackId catalog)
    harmonic = makeTransitions Harmonic (keyJumpSuccessors trackId catalog)
    tested = makeTransitions Tested (confirmedSuccessors trackId graph)
    makeTransitions = map . Transition

scoreTransitions :: MusicLibrary -> [Transition] -> [(Transition, Score)]
scoreTransitions musicLibrary values = zip values (map (scoreTransition musicLibrary) values)

scoreTransition :: MusicLibrary -> Transition -> Score
scoreTransition _ (Transition Tested _) = 0
scoreTransition _ (Transition SameKey _) = 1
scoreTransition _ (Transition Harmonic _) = 2

-- Built-in data
another_summer_id = 0

how_could_i_say_it_id = 1

nothing_is_impossible_id = 2

spirit_id = 3

mahon_id = 4

howl_at_the_moon_id = 5

melody_express_id = 6

tribe_of_the_sun_id = 7

inferno_id = 8

against_the_wind_id = 9

resilience_id = 10

too_much_rain_id = 11

aeterna_id = 12

set_me_free_id = 13

thunderfunk_id = 14

polkatorju_id = 15

falling_id = 16

feels_so_good_id = 17

colossal_id = 18

aniara_id = 19

marulio_id = 20

sorrento_id = 21

library :: MusicLibrary
library =
  MusicLibrary
    { nextTrackId = 100,
      trackCatalog =
        Map.fromList
          [ (another_summer_id, Track "Another Summer" (Key K_3 D)),
            (how_could_i_say_it_id, Track "How Could I say It" (Key K_10 D)),
            (nothing_is_impossible_id, Track "Nothing Is Impossible" (Key K_7 D)),
            (spirit_id, Track "Spirit" (Key K_9 D)),
            (mahon_id, Track "Mahon" (Key K_8 D)),
            (howl_at_the_moon_id, Track "Howl At The Moon" (Key K_1 M)),
            (melody_express_id, Track "Melody Express" (Key K_3 D)),
            (tribe_of_the_sun_id, Track "Tribe Of The Sun" (Key K_4 D)),
            (inferno_id, Track "Inferno" (Key K_4 D)),
            (against_the_wind_id, Track "Against The Wind" (Key K_10 D)),
            (resilience_id, Track "Resilience" (Key K_5 D)),
            (too_much_rain_id, Track "Too Much Rain" (Key K_1 D)),
            (aeterna_id, Track "Aeterna" (Key K_2 D)),
            (set_me_free_id, Track "Set Me Free" (Key K_9 M)),
            (thunderfunk_id, Track "Thunderfunk" (Key K_1 M)),
            (polkatorju_id, Track "Polkatorju" (Key K_10 D)),
            (falling_id, Track "Falling" (Key K_6 D)),
            (feels_so_good_id, Track "Feels So Good" (Key K_10 D)),
            (colossal_id, Track "Colossal" (Key K_4 M)),
            (aniara_id, Track "Aniara" (Key K_9 D)),
            (marulio_id, Track "Marulio" (Key K_1 D)),
            (sorrento_id, Track "Sorrento" (Key K_9 M))
          ],
      transitionGraph =
        Map.fromList
          [ (melody_express_id, [Edge another_summer_id]),
            (thunderfunk_id, [Edge howl_at_the_moon_id]),
            (howl_at_the_moon_id, [Edge colossal_id]),
            (nothing_is_impossible_id, [Edge mahon_id, Edge falling_id]),
            (aniara_id, [Edge how_could_i_say_it_id]),
            (feels_so_good_id, [Edge polkatorju_id]),
            (set_me_free_id, [Edge spirit_id, Edge sorrento_id]),
            (spirit_id, [Edge aniara_id]),
            (colossal_id, [Edge inferno_id]),
            (resilience_id, [Edge falling_id]),
            (mahon_id, [Edge spirit_id, Edge feels_so_good_id]),
            (against_the_wind_id, [Edge feels_so_good_id]),
            (sorrento_id, [Edge set_me_free_id, Edge spirit_id]),
            (marulio_id, [Edge melody_express_id]),
            (falling_id, [Edge mahon_id])
          ]
    }
