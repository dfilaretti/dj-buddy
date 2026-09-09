module MyLib where

import Data.Map.Strict qualified as Map
import FromYaml (makeMusicLibrary, readLibFromFile)
import MusicLibrary (MusicLibrary (..))
import Playlist (Playlist, PlaylistMember (..), mkPlayListRand)
import TestedTransitions
  ( TestedTransition (..),
  )
import TrackCatalog
  ( Track (..),
    TrackCatalog,
    TrackId,
    getTrackWithId,
  )
import Transitions
  ( Cost,
    Transition (..),
    scoredTransitions,
  )

-----------------------------------------
----- Pretty Print ----------------------
-----------------------------------------

prettyPrintTransition :: MusicLibrary -> Transition -> String
prettyPrintTransition lib (Tested t) =
  show t.to ++ ": " ++ title ++ " " ++ key ++ " [" ++ show t ++ "]"
  where
    title = case getTrackWithId t.to lib.trackCatalog of
      Just t -> t.title
      Nothing -> "ERROR"
    key = case getTrackWithId t.to lib.trackCatalog of
      Just t -> show t.key
      Nothing -> "ERROR"
prettyPrintTransition lib (Harmonic t tID) =
  show tID ++ ": " ++ title ++ " " ++ key ++ " [" ++ show t ++ "]"
  where
    title = case getTrackWithId tID lib.trackCatalog of
      Just t -> t.title
      Nothing -> "ERROR"
    key = case getTrackWithId tID lib.trackCatalog of
      Just t -> show t.key
      Nothing -> "ERROR"

printTransitions :: MusicLibrary -> [Transition] -> IO ()
printTransitions lib = putStr . unlines . map (prettyPrintTransition lib)

prettyPrintTransition' :: MusicLibrary -> (Transition, Cost) -> String
prettyPrintTransition' lib (t, c) = prettyPrintTransition lib t ++ " [cost = " ++ show c ++ "]"

printTransitions' :: MusicLibrary -> [(Transition, Cost)] -> IO ()
printTransitions' lib = putStr . unlines . map (prettyPrintTransition' lib)

printPlaylist :: MusicLibrary -> [PlaylistMember] -> String
printPlaylist lib = unlines . map prettyPrintMember
  where
    prettyPrintMember (StartingTrack tid) =
      show tid ++ ": " ++ trackDescription tid
    prettyPrintMember (MiddleTrack transition) =
      prettyPrintTransition lib transition

    trackDescription tid =
      case getTrackWithId tid lib.trackCatalog of
        Just track -> track.title ++ " " ++ show track.key
        Nothing -> "ERROR"

printStuff :: MusicLibrary -> TrackId -> String
printStuff lib tid =
  decoration ++ heading ++ msg ++ decoration
  where
    decorationLen = 100
    decoration = replicate decorationLen '-' ++ "\n"
    title = case getTrackWithId tid lib.trackCatalog of
      Just t -> t.title
      Nothing -> "ERROR"
    key = case getTrackWithId tid lib.trackCatalog of
      Just t -> t.key
      Nothing -> error "no key"
    heading = "Good transitions from \"" ++ title ++ "\" (" ++ show key ++ ")\n\n"
    msg = unlines $ map (prettyPrintTransition' lib) $ scoredTransitions lib tid

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
