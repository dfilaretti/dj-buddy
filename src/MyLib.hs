module MyLib where

import Data.Map.Strict qualified as Map
import FromYaml (makeMusicLibrary, readLibFromFile)
import MusicLibrary (MusicLibrary (..))
import Playlist (Playlist, mkPlayListRand)
import PrettyPrint
import TrackCatalog (TrackCatalog, TrackId)
import Transitions (Transition (..))

-----------------------------------------
----- Pretty Print ----------------------
-----------------------------------------

printTransitions :: MusicLibrary -> [Transition] -> IO ()
printTransitions lib = putStr . unlines . map (prettyPrintTransition lib)

printTrackCatalog :: TrackCatalog -> IO ()
printTrackCatalog =
  putStr . unlines . map formatTrack . Map.toList
  where
    formatTrack (trackId, track) =
      show trackId ++ ": " ++ show track

{---------------------------------------------------------------------------
  Music Library parsing from YAML
----------------------------------------------------------------------------}

{---------------------------------------------------------------------------
  Sample IO actions
----------------------------------------------------------------------------}

mkRandomPlaylist :: Int -> IO ()
mkRandomPlaylist size =
  do
    rawData <- readLibFromFile "src/library.yaml"
    let lib = makeMusicLibrary rawData
    playlist <- mkPlayListRand lib size
    putStr $ printPlaylist lib playlist

{---------------------------------------------------------------------------
  TODO
----------------------------------------------------------------------------}

bestTransitions ::
  TrackId -> -- optional starting track
  Int -> -- number of transitions
  IO [Transition] -- we may want to shuffle "same score" items here
bestTransitions = undefined

gimmePlaylist ::
  Maybe TrackId ->
  Int ->
  IO Playlist
gimmePlaylist = undefined
