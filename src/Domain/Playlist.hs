module Domain.Playlist (mkPlayListRand, mkPlayList, PlaylistMember (..), Playlist) where

import Data.Map.Strict qualified as Map
import Domain.MusicLibrary
import Domain.TrackCatalog
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

getTraskIds_ :: Playlist -> [TrackId]
getTraskIds_ = fmap getTrackId_

mkPlayListRand ::
  MusicLibrary ->
  Int -> -- number of tracks
  IO Playlist
mkPlayListRand lib n =
  do
    tid <- randomElement (Map.keys lib.trackCatalog)
    case tid of
      Just xoxo -> mkPlayList lib xoxo n
      Nothing -> error "ouch"

mkPlayList ::
  MusicLibrary ->
  TrackId -> -- starting track
  Int -> -- number of tracks
  IO Playlist
mkPlayList lib tid n =
  mkPlayList_ lib tid n [StartingTrack tid]

mkPlayList_ ::
  MusicLibrary ->
  TrackId -> -- starting track
  Int -> -- number of tracks
  Playlist -> -- accumulated result
  IO Playlist
mkPlayList_ lib tid n ts
  | length ts == n = return ts
  | otherwise =
      do
        trans <- pickBest allTrans
        case trans of
          Nothing -> return ts
          Just ((trans_, _)) ->
            mkPlayList_ lib (destTrackId trans_) n (ts ++ [MiddleTrack trans_])
  where
    allTrans = filter notUsed $ scoredTransitions lib tid
    usedTracks = getTraskIds_ ts
    notUsed (t, _) = not $ destTrackId t `elem` usedTracks
