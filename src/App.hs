module App (mkPlaylist, getTracks, ResolvedPlaylist, ResolvedPlaylistMember (..)) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Domain.Camelot (Key)
import Domain.MusicLibrary (MusicLibrary (..))
import Domain.Playlist (Playlist)
import Domain.Playlist qualified as P
import Domain.TrackCatalog (Track (..), TrackCatalog, TrackId)
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
  Helper Functions
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

mkPlaylist_ ::
  MusicLibrary ->
  Maybe TrackId -> -- optional starting track (random if not provided)
  Maybe Int -> -- number of tracks (default = 10)
  IO Playlist
mkPlaylist_ lib mtid mn =
  case mtid of
    Nothing -> P.mkPlayListRand lib n
    Just tid -> P.mkPlayList lib tid n
  where
    n = case mn of
      Nothing -> 10
      Just n_ -> n_

{---------------------------------------------------------------------------
  Exported Functions
----------------------------------------------------------------------------}

getTracks ::
  MusicLibrary ->
  Maybe Text ->
  Maybe Key ->
  TrackCatalog
getTracks lib title key =
  Map.filter keyPred $ Map.filter titlePred lib.trackCatalog
  where
    trackTitleContains s t =
      T.isInfixOf (T.toLower s) (T.toLower t.title)
    trackHasKey k t = k == t.key
    titlePred = case title of
      Nothing -> const True
      Just s -> trackTitleContains s
    keyPred = case key of
      Nothing -> const True
      Just k_ -> trackHasKey k_

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
  IO ResolvedPlaylist
mkPlaylist lib mtid mn =
  do
    p <- mkPlaylist_ lib mtid mn
    case resolvePlaylist lib p of
      Just resolvedPlaylist -> return resolvedPlaylist
      Nothing -> error "Unable to resolve playlist; this shouldn't happen, check your data!"
