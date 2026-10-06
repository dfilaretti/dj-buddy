module Main (main) where

import App (getTrackFromId, getTracks, getTransitions, mkPlaylist)
import CLI.Render
import Data.Text (Text)
import Domain.Camelot (Key)
import Domain.TrackCatalog (TrackId)
import Options.Applicative
import Persistence.Yaml (readLibFromFile_)

{---------------------------------------------------------------------------
  CLI parser
----------------------------------------------------------------------------}

data Command
  = MakePlaylist MakePlaylistOptions
  | SuggestNextTracks SuggestNextTracksOptions
  | ShowLibrary ShowLibraryOptions
  deriving (Show)

data MakePlaylistOptions = MakePlaylistOptions
  { libraryPath :: Maybe String,
    firstTrack :: Maybe TrackId,
    maxTracks :: Int
  }
  deriving (Show)

playlistParser :: ParserInfo Command
playlistParser =
  info
    ( pure MakePlaylist
        <*> ( pure MakePlaylistOptions
                <*> optional
                  ( strOption
                      ( long "lib"
                          <> short 'l'
                          <> metavar "FILE"
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

data SuggestNextTracksOptions = SuggestNextTracksOptions
  { libraryPath :: Maybe String,
    track :: TrackId,
    maxTracks :: Int
  }
  deriving (Show)

suggestParser :: ParserInfo Command
suggestParser =
  info
    ( pure SuggestNextTracks
        <*> ( pure SuggestNextTracksOptions
                <*> optional
                  ( strOption
                      ( long "lib"
                          <> short 'l'
                          <> metavar "FILE"
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

data ShowLibraryOptions = ShowLibraryOptions
  { libraryPath :: Maybe String,
    title :: Maybe Text,
    key :: Maybe Key
  }
  deriving (Show)

showLibraryParser :: ParserInfo Command
showLibraryParser =
  info
    ( pure ShowLibrary
        <*> ( pure ShowLibraryOptions
                <*> optional
                  ( strOption
                      ( long "lib"
                          <> short 'l'
                          <> metavar "FILE"
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

commandParser :: Parser Command
commandParser =
  subparser $
    (command "playlist" playlistParser)
      <> (command "suggest" suggestParser)
      <> (command "show" showLibraryParser)

parseCliCommand :: IO Command
parseCliCommand =
  execParser $
    info (helper <*> commandParser) $
      (progDesc "Helps you become a big DJ")

handleMakePlaylist :: MakePlaylistOptions -> IO ()
handleMakePlaylist opts =
  do
    lib <- readLibFromFile_ (getLibraryPath opts.libraryPath)
    plist <- mkPlaylist lib opts.firstTrack opts.maxTracks
    putStr $ renderResolvedPlaylist plist

handleShowLibrary :: ShowLibraryOptions -> IO ()
handleShowLibrary o =
  do
    lib <- readLibFromFile_ (getLibraryPath o.libraryPath)
    putStr $ renderTrackCatalog (getTracks lib o.title o.key)

handleShowSuggest :: SuggestNextTracksOptions -> IO ()
handleShowSuggest o =
  do
    lib <- readLibFromFile_ (getLibraryPath o.libraryPath)
    let startingTrack =
          maybe
            -- TODO: is this the right way to handle errors?!
            (error "Init track not found; this shouldn't happen")
            show
            (getTrackFromId lib o.track)
    putStrLn $ "Showing suggestions for: " ++ startingTrack
    putStrLn $ replicate 60 '-'
    putStr $ renderResolvedTransitions (getTransitions lib o.track o.maxTracks)

getLibraryPath :: Maybe String -> String
getLibraryPath x = maybe "src/library.yaml" id x

runCommand :: Command -> IO ()
runCommand c =
  case c of
    MakePlaylist o -> handleMakePlaylist o
    SuggestNextTracks o -> handleShowSuggest o
    ShowLibrary o -> handleShowLibrary o

{---------------------------------------------------------------------------
  Main
----------------------------------------------------------------------------}

main :: IO ()
main = do
  c <- parseCliCommand
  runCommand c
