module Domain.Playlist (mkPlayListRand, mkPlayList, getTrackId_, getTrack_, PlaylistMember (..), Playlist) where

import Domain.MusicLibrary
import Domain.Transitions
import Domain.Utils (randomElement)

data PlaylistMember
  = StartingTrack TrackId
  | MiddleTrack Transition
  deriving (Show)

type Playlist = [PlaylistMember]

getTrackId_ :: PlaylistMember -> TrackId
getTrackId_ (StartingTrack tid) = tid
getTrackId_ (MiddleTrack t) = destTrackId t

getTrack_ :: MusicLibrary -> PlaylistMember -> Maybe Track
getTrack_ lib pm = getTrackWithId (getTrackId_ pm) lib

mkPlayListRand ::
  MusicLibrary ->
  Int -> -- number of tracks
  IO Playlist
mkPlayListRand lib n =
  do
    maybeTid <- randomElement (getTrackIds lib)
    case maybeTid of
      Just tid -> mkPlayList lib tid n
      Nothing -> pure [] -- TODO: return error instead (using Either)?

mkPlayList ::
  MusicLibrary ->
  TrackId -> -- starting track
  Int -> -- number of tracks
  IO Playlist
mkPlayList lib start n
  | n <= 0 = pure []
  | otherwise =
      (StartingTrack start :) <$> go start (n - 1) [start]
  where
    go :: TrackId -> Int -> [TrackId] -> IO Playlist
    go _ 0 _ = pure []
    go current remaining used = do
      chosen <-
        pickBest $
          filter (\(t, _) -> destTrackId t `notElem` used) $
            scoredTransitions lib current
      case chosen of
        Nothing -> pure []
        Just (t, _) ->
          let next = destTrackId t
           in (MiddleTrack t :) <$> go next (remaining - 1) (next : used)
