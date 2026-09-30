module CLI.Render where

import Data.Map.Strict qualified as Map
import Domain.MusicLibrary
import Domain.Playlist
import Domain.TestedTransitions (TestedTransition (to))
import Domain.TrackCatalog
import Domain.Transitions (Cost, Transition (..), scoredTransitions)
import Persistence.Yaml

{-
TODO: The stuff in here should eventually go into the CLI app.
The lib returns things like Playlist and Transition; the CLI app is
responsible for the "pretty printing" and stuff like that.
Similarly, the Web app will be responsible for producing JSON etc.
-}

prettyPrintTrackCatalog :: TrackCatalog -> String
prettyPrintTrackCatalog =
  unlines . map formatTrack . Map.toList
  where
    formatTrack (trackId, track) =
      show trackId ++ ": " ++ show track

prettyPrintTransitions :: MusicLibrary -> [Transition] -> String
prettyPrintTransitions lib = unlines . map (prettyPrintTransition lib)

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

prettyPrintTransition' :: MusicLibrary -> (Transition, Cost) -> String
prettyPrintTransition' lib (t, c) = prettyPrintTransition lib t ++ " [cost = " ++ show c ++ "]"

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

-- TODO: in theory this should only use the "application level" functions exposed by the lib... plus the pretty printing stuff (which eventually will live in the CLI)
showTrackCatalog :: TrackCatalog -> IO ()
showTrackCatalog =
  putStr . prettyPrintTrackCatalog

-- TODO: this should call mkPlaylist (or appropriate function) from "application layer"
showRandomPlaylist :: Int -> IO ()
showRandomPlaylist size =
  do
    rawData <- readLibFromFile "src/library.yaml"
    let lib = makeMusicLibrary rawData
    playlist <- mkPlayListRand lib size
    putStr $ printPlaylist lib playlist
