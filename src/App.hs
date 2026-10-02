module App (mkPlaylist) where

import Domain.MusicLibrary (MusicLibrary (..))
import Domain.Playlist (Playlist)
import Domain.Playlist qualified as P
import Domain.TrackCatalog (Track, TrackId)
import Domain.Transitions (Transition (..))

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
  Maybe Int -> -- number of tracks (default = 10)
  IO Playlist
mkPlaylist lib mtid mn =
  case mtid of
    Nothing -> P.mkPlayListRand lib n
    Just tid -> P.mkPlayList lib tid n
  where
    n = case mn of
      Nothing -> 10
      Just n_ -> n_
