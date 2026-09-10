module CLIHelpers where

import TrackCatalog
import Playlist
import PrettyPrint
import FromYaml

{---------------------------------------------------------------------------
  Draft CLI tools; mode to CLI when we do it.

  Those can be use to quicly try things out on the REPL while we don't
  have a proper CLI implemented.
----------------------------------------------------------------------------}

-- TODO: in theory this should only use the "application level" functions exposed by the lib... plus the pretty printing stuff (which eventually will live in the CLI)
showTrackCatalog :: TrackCatalog -> IO ()
showTrackCatalog =
  putStr . prettyPrintTrackCatalog

-- TODO: this should call mkPlaylist below and basically just prety print the rrsult
showRandomPlaylist :: Int -> IO ()
showRandomPlaylist size =
  do
    rawData <- readLibFromFile "src/library.yaml"
    let lib = makeMusicLibrary rawData
    playlist <- mkPlayListRand lib size
    putStr $ printPlaylist lib playlist
