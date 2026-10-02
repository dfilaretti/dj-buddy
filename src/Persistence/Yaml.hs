module Persistence.Yaml where

import Data.Yaml (FromJSON, decodeFileThrow)
import Domain.Camelot
import Domain.MusicLibrary
import Domain.TestedTransitions
import Domain.TrackCatalog (Track (..), TrackCatalog, TrackId, addTrack, emptyCatalog)
import GHC.Generics (Generic)

readLibFromFile :: FilePath -> IO Lib_raw
readLibFromFile p = decodeFileThrow p :: IO Lib_raw

readLibFromFile_ :: FilePath -> IO MusicLibrary
readLibFromFile_ p =
  do
    parsedYamlLib <- readLibFromFile p
    return (makeMusicLibrary parsedYamlLib)

data Track_raw = Track_raw
  { id :: TrackId,
    title :: String,
    key :: Key
  }
  deriving (Show, Generic)

instance FromJSON Track_raw

data Transition_raw = Transition_raw
  { from :: TrackId,
    to :: TrackId,
    notes :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON Transition_raw

data Lib_raw = Lib_raw
  { tracks :: [Track_raw],
    transitions :: [Transition_raw]
  }
  deriving (Show, Generic)

instance FromJSON Lib_raw

makeTrackCatalogEntry :: Track_raw -> (TrackId, Track)
makeTrackCatalogEntry t_raw =
  (t_raw.id, Track {title = t_raw.title, key = t_raw.key})

makeTrackCatalog :: [Track_raw] -> TrackCatalog
makeTrackCatalog = foldr insertTrack emptyCatalog

insertTrack :: Track_raw -> TrackCatalog -> TrackCatalog
insertTrack tr c = addTrack (makeTrackCatalogEntry tr) c

makeTransitionGraphEntry :: Transition_raw -> (TrackId, TestedTransition)
makeTransitionGraphEntry t =
  (t.from, TestedTransition {to = t.to, notes = t.notes})

makeTransitionGraph :: [Transition_raw] -> MusicLibrary -> MusicLibrary
makeTransitionGraph ts lib = foldr insertRawTransition lib ts

insertRawTransition :: Transition_raw -> MusicLibrary -> MusicLibrary
insertRawTransition tx lib =
  insertTransition from t lib
  where
    (from, t) = makeTransitionGraphEntry tx

makeMusicLibrary :: Lib_raw -> MusicLibrary
makeMusicLibrary l =
  makeTransitionGraph l.transitions intial
  where
    intial =
      MusicLibrary
        { trackCatalog = makeTrackCatalog l.tracks,
          transitionGraph = emptyGraph
        }
