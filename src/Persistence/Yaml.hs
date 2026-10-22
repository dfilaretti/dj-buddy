module Persistence.Yaml where

import Control.Monad (foldM)
import Data.Text (Text)
import Data.Yaml (FromJSON, decodeFileEither)
import Domain.Camelot
import Domain.MusicLibrary
import GHC.Generics (Generic)

data YamlError
  = MusicLibraryError MusicLibraryError
  | YamlFileNotFound FilePath
  deriving (Show, Eq)

readLibFromFile :: FilePath -> IO (Either YamlError Lib_raw)
readLibFromFile yamlFile = do
  parsedYaml <- decodeFileEither yamlFile
  case parsedYaml of
    Left _ -> pure $ Left $ YamlFileNotFound yamlFile
    Right rawLib -> pure $ Right rawLib

readMusicLibrary :: FilePath -> IO (Either YamlError MusicLibrary)
readMusicLibrary filename = do
  rawLibLoadResult <- readLibFromFile filename
  case rawLibLoadResult of
    Left e -> pure $ Left e
    Right rawLib ->
      case makeMusicLibrary rawLib of
        Left e -> pure $ Left $ e
        Right lib -> pure $ Right lib

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

-- instance FromJSON TrackId

data Lib_raw = Lib_raw
  { tracks :: [Track_raw],
    transitions :: [Transition_raw]
  }
  deriving (Show, Generic)

instance FromJSON Lib_raw

makeTrackCatalogEntry :: Track_raw -> (TrackId, Track)
makeTrackCatalogEntry t_raw =
  (t_raw.id, Track {title = t_raw.title, key = t_raw.key})

makeTrackCatalog :: [Track_raw] -> Either YamlError MusicLibrary
makeTrackCatalog rawTracks = foldM (\current t -> insertTrack t current) emptyMusicLibrary rawTracks

insertTrack :: Track_raw -> MusicLibrary -> Either YamlError MusicLibrary
insertTrack tr c =
  case withTrack (makeTrackCatalogEntry tr) c of
    Left e -> Left $ MusicLibraryError e
    Right c' -> Right c'

makeTransitionGraphEntry :: Transition_raw -> (TrackId, TestedTransition)
makeTransitionGraphEntry t =
  (t.from, TestedTransition {notes = t.notes})

makeTransitionGraph :: [Transition_raw] -> MusicLibrary -> Either YamlError MusicLibrary
makeTransitionGraph ts lib =
  foldM (\current tx -> insertRawTransition tx current) lib ts

insertRawTransition ::
  Transition_raw ->
  MusicLibrary ->
  Either YamlError MusicLibrary
insertRawTransition tx lib =
  case withTransition tx.from tx.to t lib of
    Left e -> Left (MusicLibraryError e)
    Right newLib -> Right newLib
  where
    (_, t) = makeTransitionGraphEntry tx

makeMusicLibrary :: Lib_raw -> Either YamlError MusicLibrary
makeMusicLibrary l =
  case makeTrackCatalog l.tracks of
    Left e -> Left e
    Right cat ->
      makeTransitionGraph
        l.transitions
        cat
