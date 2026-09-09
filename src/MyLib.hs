module MyLib where

import FromYaml (makeMusicLibrary, readLibFromFile)
import MusicLibrary (MusicLibrary (..))
import Playlist (Playlist, mkPlayListRand)
import PrettyPrint
import TrackCatalog (Track, TrackCatalog, TrackId)
import Transitions (Transition (..))

{---------------------------------------------------------------------------
  Draft CLI tools; mode to CLI when we do it.

  Those can be use to quicly try things out on the REPL while we don't
  have a proper CLI implemented.
----------------------------------------------------------------------------}

-- TODO: in theory this should only use the "application level" functions exposed by the lib... plus the pretty printing stuff (which eventually will live in the CLI)
showTrackCatalog :: TrackCatalog -> IO ()
showTrackCatalog =
  putStr . prettyPrintTrackCatalog

-- TODO: this should call mkPlaylist below and basically just prety print the rrsult
showRandomPlaylist :: Int -> IO ()
showRandomPlaylist size =
  do
    rawData <- readLibFromFile "src/library.yaml"
    let lib = makeMusicLibrary rawData
    playlist <- mkPlayListRand lib size
    putStr $ printPlaylist lib playlist

{---------------------------------------------------------------------------
  TODO: Application level; this is what CLI and web UI will use
----------------------------------------------------------------------------}

getTracks ::
  MusicLibrary ->
  [(TrackId, Track)] -- TODO: sure we want this? maybe add the ID into track and have a simple list instead?
getTracks = undefined

getTransitions ::
  MusicLibrary ->
  TrackId -> -- origin track
  Int -> -- desired number of alternative transitions
  IO [Transition] -- IO coz we may want to shuffle same-score items here
getTransitions = undefined

mkPlaylist ::
  MusicLibrary ->
  Maybe TrackId -> -- optional starting track (random if not provided)
  Int -> -- number of tracks
  IO Playlist
mkPlaylist = undefined
