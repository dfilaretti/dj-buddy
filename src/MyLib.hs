module MyLib where

import FromYaml (makeMusicLibrary, readLibFromFile)
import MusicLibrary (MusicLibrary (..))
import Playlist (Playlist, mkPlayListRand)
import PrettyPrint
import TrackCatalog (Track, TrackCatalog, TrackId)
import Transitions (Transition (..))

showTrackCatalog :: TrackCatalog -> IO ()
showTrackCatalog =
  putStr . prettyPrintTrackCatalog

showRandomPlaylist :: Int -> IO ()
showRandomPlaylist size =
  do
    rawData <- readLibFromFile "src/library.yaml"
    let lib = makeMusicLibrary rawData
    playlist <- mkPlayListRand lib size
    putStr $ printPlaylist lib playlist

{---------------------------------------------------------------------------
  TODO
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
