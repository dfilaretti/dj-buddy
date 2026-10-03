module App (mkPlaylist) where

import Domain.MusicLibrary (MusicLibrary (..))
import Domain.MusicLibrary qualified as L
import Domain.MusicLibrary qualified as P
import Domain.Playlist (Playlist)
import Domain.Playlist qualified as P
import Domain.TrackCatalog (Track, TrackId)
import Domain.Transitions (Transition (..))

{---------------------------------------------------------------------------
  Data
----------------------------------------------------------------------------}

data ResolvedPlaylistMember = ResolvedPlaylistMember
  { playlistMember :: P.PlaylistMember,
    track :: Track
  }
  deriving (Show)

type ResolvedPlaylist = [ResolvedPlaylistMember]

{---------------------------------------------------------------------------
  Functions
----------------------------------------------------------------------------}

-- TODO: refactor this! I'm sure it can be simplified using fmap etc.
resolvePlaylistMember ::
  MusicLibrary ->
  P.PlaylistMember ->
  Maybe ResolvedPlaylistMember
resolvePlaylistMember lib pm =
  case P.getTrack_ lib pm of
    Just t -> Just (ResolvedPlaylistMember pm t)
    Nothing -> Nothing

resolvePlaylist ::
  MusicLibrary ->
  P.Playlist ->
  Maybe ResolvedPlaylist
resolvePlaylist lib p = sequence $ fmap (resolvePlaylistMember lib) p

{---------------------------------------------------------------------------
  Functions
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
