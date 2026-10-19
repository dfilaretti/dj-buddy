module Persistence.Yaml where

import Control.Monad (foldM)
import Data.Text (Text)
import Data.Yaml (FromJSON, decodeFileThrow)
import Domain.Camelot
import Domain.MusicLibrary
import GHC.Generics (Generic)

data YamlError
  = MusicLibraryError MusicLibraryError
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
  case withTransition_ tx.from tx.to t lib of
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
        cat
