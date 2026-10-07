module Persistence.Yaml where

import Control.Monad (foldM)
import Data.Text (Text)
import Data.Yaml (FromJSON, decodeFileThrow)
import Domain.Camelot
import Domain.MusicLibrary
import Domain.TestedTransitions
import Domain.TrackCatalog (Track (..), TrackCatalog, TrackCatalogError, TrackId, addTrack, emptyCatalog)
import GHC.Generics (Generic)

data YamlError
  = MusicLibraryError MusicLibraryError
  | TrackCatalogError TrackCatalogError
  deriving (Show, Eq)

readLibFromFile :: FilePath -> IO Lib_raw
readLibFromFile p = decodeFileThrow p :: IO Lib_raw

readLibFromFile_ :: FilePath -> IO (Either YamlError MusicLibrary)
readLibFromFile_ p =
  fmap makeMusicLibrary (readLibFromFile p)

data Track_raw = Track_raw
  { id :: TrackId,
    title :: Text,
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

makeTrackCatalog :: [Track_raw] -> Either YamlError TrackCatalog
makeTrackCatalog rawTracks = foldM (\current t -> insertTrack t current) emptyCatalog rawTracks

insertTrack :: Track_raw -> TrackCatalog -> Either YamlError TrackCatalog
insertTrack tr c =
  case addTrack (makeTrackCatalogEntry tr) c of
    Left e -> Left $ TrackCatalogError e
    Right c' -> Right c'

makeTransitionGraphEntry :: Transition_raw -> (TrackId, TestedTransition)
makeTransitionGraphEntry t =
  (t.from, TestedTransition {to = t.to, notes = t.notes})

makeTransitionGraph :: [Transition_raw] -> MusicLibrary -> Either YamlError MusicLibrary
makeTransitionGraph ts lib =
  foldM (\current tx -> insertRawTransition tx current) lib ts

insertRawTransition ::
  Transition_raw ->
  MusicLibrary ->
  Either YamlError MusicLibrary
insertRawTransition tx lib =
  case insertTransition from t lib of
    Left e -> Left (MusicLibraryError e)
    Right newLib -> Right newLib
  where
    (from, t) = makeTransitionGraphEntry tx

makeMusicLibrary :: Lib_raw -> Either YamlError MusicLibrary
makeMusicLibrary l =
  case makeTrackCatalog l.tracks of
    Left e -> Left e
    Right cat ->
      makeTransitionGraph
        l.transitions
        MusicLibrary {trackCatalog = cat, transitionGraph = emptyGraph}
