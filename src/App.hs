module App (mkPlaylist, getTracks, getTransitions, ResolvedPlaylist, ResolvedPlaylistMember (..), ResolvedTransition (..)) where

import Data.Text (Text)
import Domain.Camelot (Key)
import Domain.MusicLibrary (MusicLibrary (..), Track (..), TrackId)
import Domain.MusicLibrary qualified as L
import Domain.Playlist (Playlist)
import Domain.Playlist qualified as P
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
  Errors
----------------------------------------------------------------------------}

data AppError
  = TrackNotFound TrackId
  | EmptyLibrary
  | CouldNotResolvePlaylistMember TrackId
  | CouldNotResolveTransition Transition
  deriving (Show, Eq)

{---------------------------------------------------------------------------
  Helper Functions
----------------------------------------------------------------------------}

requireTrack ::
  MusicLibrary ->
  TrackId ->
  Either AppError Track
requireTrack lib trackId =
  maybe
    (Left (TrackNotFound trackId))
    Right
    (L.getTrackWithId trackId lib)

-- TODO: refactor this! I'm sure it can be simplified using fmap etc.
resolvePlaylistMember ::
  MusicLibrary ->
  P.PlaylistMember ->
  Either AppError ResolvedPlaylistMember
resolvePlaylistMember lib pm =
  case P.getTrack_ lib pm of
    Just t -> Right (ResolvedPlaylistMember pm t)
    Nothing -> Left $ CouldNotResolvePlaylistMember (P.getTrackId_ pm)

resolveTransition ::
  MusicLibrary ->
  Transition ->
  Either AppError ResolvedTransition
resolveTransition lib t =
  case T.destTrack lib t of
    Just tr -> Right (ResolvedTransition t tr)
    Nothing -> Left (CouldNotResolveTransition t)

resolvePlaylist ::
  MusicLibrary ->
  P.Playlist ->
  Either AppError ResolvedPlaylist
resolvePlaylist lib p = sequence $ fmap (resolvePlaylistMember lib) p

mkPlaylist_ ::
  MusicLibrary ->
  Maybe TrackId -> -- optional starting track (random if not provided)
  Int -> -- number of tracks (default = 10)
  IO Playlist
mkPlaylist_ lib mtid n =
  case mtid of
    Nothing -> P.mkPlayListRand lib n
    Just tid -> P.mkPlayList lib tid n

{---------------------------------------------------------------------------
  Exported Functions
----------------------------------------------------------------------------}

getTracks ::
  MusicLibrary ->
  Maybe Text ->
  Maybe Key ->
  MusicLibrary
getTracks lib title key =
  L.filterLib (\x -> all ($ x) [keyPred, titlePred]) lib
  where
    titlePred = maybe (const True) L.titleMatches title
    keyPred = maybe (const True) L.keyMatches key

getTransitions ::
  MusicLibrary ->
  TrackId -> -- origin track
  Int -> -- desired number of alternative transitions
  Either AppError [ResolvedTransition]
getTransitions lib tid n =
  do
    _ <- requireTrack lib tid
    sequence $ fmap (resolveTransition lib) $ map fst $ take n $ T.scoredTransitions lib tid

mkPlaylist ::
  MusicLibrary ->
  Maybe TrackId -> -- optional starting track (random if not provided)
  Int -> -- number of tracks
  IO (Either AppError ResolvedPlaylist)
mkPlaylist lib mtid n =
  do
    p <- mkPlaylist_ lib mtid n
    return $ resolvePlaylist lib p
