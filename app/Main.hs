module Main (main) where

import App (getTracks, getTransitions, mkPlaylist)
import CLI.Render
import Data.Text (Text)
import Domain.Camelot (Key)
import Domain.MusicLibrary (TrackId)
import Options.Applicative
import Persistence.Yaml (readLibFromFile_)

{---------------------------------------------------------------------------
  Command data structure
----------------------------------------------------------------------------}

data Command
  = MakePlaylist MakePlaylistOptions
  | SuggestNextTracks SuggestNextTracksOptions
  | ShowLibrary ShowLibraryOptions
  deriving (Show)

data MakePlaylistOptions = MakePlaylistOptions
  { libraryPath :: String,
    firstTrack :: Maybe TrackId,
    maxTracks :: Int
  }
  deriving (Show)

data SuggestNextTracksOptions = SuggestNextTracksOptions
  { libraryPath :: String,
    track :: TrackId,
    maxTracks :: Int
  }
  deriving (Show)

data ShowLibraryOptions = ShowLibraryOptions
  { libraryPath :: String,
    title :: Maybe Text,
    key :: Maybe Key
  }
  deriving (Show)

{---------------------------------------------------------------------------
  CLI parser
----------------------------------------------------------------------------}

commandParser :: Parser Command
commandParser =
  subparser $
    (command "playlist" makePlaylistParser)
      <> (command "suggest" suggestNextTracksParser)
      <> (command "show" showLibraryParser)

makePlaylistParser :: ParserInfo Command
makePlaylistParser =
  info
    ( pure MakePlaylist
        <*> ( pure MakePlaylistOptions
                <*> ( strOption
                        ( long "lib"
                            <> short 'l'
                            <> metavar "FILE"
                            <> value defaultLibraryPath
                            <> help "Path to the music library YAML file"
                        )
                    )
                <*> optional
                  ( strOption -- technically, this should be TrackID
                      ( long "startTrack"
                          <> short 's'
                          <> metavar "TRACK_ID"
                          <> help "Track ID to use as the first track"
                      )
                  )
                <*> ( option
                        auto
                        ( long "numTracks"
                            <> short 'n'
                            <> value 10
                            <> metavar "COUNT"
                            <> help "Maximum number of tracks to include"
                        )
                    )
            )
    )
    (progDesc "Generate a sample playlist")

suggestNextTracksParser :: ParserInfo Command
suggestNextTracksParser =
  info
    ( pure SuggestNextTracks
        <*> ( pure SuggestNextTracksOptions
                <*> ( strOption
                        ( long "lib"
                            <> short 'l'
                            <> metavar "FILE"
                            <> value defaultLibraryPath
                            <> help "Path to the music library YAML file"
                        )
                    )
                <*> strArgument
                  ( metavar "TRACK_ID"
                      <> help "Track ID to find suggestions for"
                  )
                <*> ( option
                        auto
                        ( long "numTracks"
                            <> short 'n'
                            <> metavar "COUNT"
                            <> value 3
                            <> showDefault
                            <> help "Maximum number of tracks to show"
                        )
                    )
            )
    )
    (progDesc "Suggest suitable next tracks")

showLibraryParser :: ParserInfo Command
showLibraryParser =
  info
    ( pure ShowLibrary
        <*> ( pure ShowLibraryOptions
                <*> ( strOption
                        ( long "lib"
                            <> short 'l'
                            <> metavar "FILE"
                            <> value defaultLibraryPath
                            <> help "Path to the music library YAML file"
                        )
                    )
                <*> optional
                  ( strOption
                      ( long "title"
                          <> short 't'
                          <> metavar "TITLE"
                          <> help "Title (or part of)"
                      )
                  )
                <*> optional
                  ( option
                      auto
                      ( long "key"
                          <> short 'k'
                          <> metavar "KEY"
                          <> help "Key of the tracks to show"
                      )
                  )
            )
    )
    (progDesc "Lists tracks in the library")

{---------------------------------------------------------------------------
  Helpers
----------------------------------------------------------------------------}

defaultLibraryPath :: String
defaultLibraryPath = "src/library.yaml"

handleMakePlaylist :: MakePlaylistOptions -> IO ()
handleMakePlaylist opts =
  do
    lib <- readLibFromFile_ opts.libraryPath
    case lib of
      Left e -> print e
      Right lib' -> do
        plist <- mkPlaylist lib' opts.firstTrack opts.maxTracks
        case plist of
          Left e -> print e
          Right plist' -> putStr $ renderResolvedPlaylist plist'

handleShowLibrary :: ShowLibraryOptions -> IO ()
handleShowLibrary o =
  do
    lib <- readLibFromFile_ o.libraryPath
    case lib of
      Left e -> print e
      Right l_ ->
        putStr $ renderTrackCatalog (getTracks l_ o.title o.key)

handleSuggestNextTracks :: SuggestNextTracksOptions -> IO ()
handleSuggestNextTracks o =
  do
    lib <- readLibFromFile_ o.libraryPath
    case lib of
      Left e -> print e
      Right l_ ->
        case getTransitions l_ o.track o.maxTracks of
          Left e -> print e
          Right ts -> showTs ts
  where
    showTs ts = putStr $ renderResolvedTransitions ts

{---------------------------------------------------------------------------
  Main
----------------------------------------------------------------------------}

runCommand :: Command -> IO ()
runCommand c =
  case c of
    MakePlaylist o -> handleMakePlaylist o
    SuggestNextTracks o -> handleSuggestNextTracks o
    ShowLibrary o -> handleShowLibrary o

parseCliCommand :: IO Command
parseCliCommand =
  execParser $
    info (helper <*> commandParser) $
      (progDesc "Helps you become a big DJ")

main :: IO ()
main = do
  c <- parseCliCommand
  runCommand c
