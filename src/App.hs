module App (mkPlaylist, getTracks, getTransitions, ResolvedPlaylist, ResolvedPlaylistMember (..), ResolvedTransition (..)) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Domain.Camelot (Key)
import Domain.MusicLibrary (MusicLibrary (..))
import Domain.Playlist (Playlist)
import Domain.Playlist qualified as P
import Domain.TrackCatalog (Track (..), TrackCatalog, TrackId)
import Domain.TrackCatalog qualified as C
import Domain.Transitions (Transition (..))
import Domain.Transitions qualified as T

{---------------------------------------------------------------------------
  Data
----------------------------------------------------------------------------}

data ResolvedTransition = ResolvedTransition
  { transition :: Transition,
    track :: Track
  }
  deriving (Show)

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

resolveTransition ::
  MusicLibrary ->
  Transition ->
  Maybe ResolvedTransition
resolveTransition lib t =
  case T.destTrack lib.trackCatalog t of
    Just tr -> Just (ResolvedTransition t tr)
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
  Map.filter
    (\x -> all ($ x) [keyPred, titlePred])
    lib.trackCatalog
  where
    titlePred = maybe (const True) C.titleMatches title
    keyPred = maybe (const True) C.keyMatches key

getTransitions ::
  MusicLibrary ->
  TrackId -> -- origin track
  Int -> -- desired number of alternative transitions
  [ResolvedTransition]
getTransitions lib tid n =
  case resolved of
    Just ts -> ts
    Nothing -> error "could not resolve transitions. something went wrong!"
  where
    ts = fmap (resolveTransition lib) $ map fst $ take n $ T.scoredTransitions lib tid
    resolved = sequence ts

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
