module Main (main) where

import App (mkResolvedPlaylist)
import CLI.Render
import Domain.Camelot (Key)
import Domain.TrackCatalog (TrackId)
import Options.Applicative
import Persistence.Yaml (readLibFromFile_)

data MakePlaylistOptions = MakePlaylistOptions
  { libraryPath :: Maybe String,
    firstTrack :: Maybe TrackId,
    maxTracks :: Maybe Int
  }
  deriving (Show)

data SuggestNextTracksOptions = SuggestNextTracksOptions
  { track :: TrackId,
    maxTracks :: Maybe Int
  }
  deriving (Show)

data ShowLibraryOptions = ShowLibraryOptions
  { maxTracks :: Maybe Int,
    titleFilter :: Maybe String,
    keyFilter :: Maybe Key
  }
  deriving (Show)

data Command
  = MakePlaylist MakePlaylistOptions
  | SuggestNextTracks SuggestNextTracksOptions
  | ShowLibrary ShowLibraryOptions
  deriving (Show)

playlistParser :: Parser Command
playlistParser =
  pure MakePlaylist
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
            <*> optional
              ( option
                  auto
                  ( long "numTracks"
                      <> short 'n'
                      <> metavar "COUNT"
                      <> help "Maximum number of tracks to include"
                  )
              )
        )

playlistParserInfo :: ParserInfo Command
playlistParserInfo =
  info
    playlistParser
    (progDesc "Generate a sample playlist")

suggestParser :: ParserInfo Command
suggestParser =
  info
    (pure $ SuggestNextTracks (SuggestNextTracksOptions "foo_id" Nothing))
    (progDesc "Suggest suitable next tracks")

showLibraryParser :: ParserInfo Command
showLibraryParser =
  info
    (pure $ ShowLibrary (ShowLibraryOptions {maxTracks = Nothing, titleFilter = Nothing, keyFilter = Nothing}))
    (progDesc "Generate a sample playlist")

commandParser :: Parser Command
commandParser =
  subparser $
    (command "playlist" playlistParserInfo)
      <> (command "suggest" suggestParser)
      <> (command "show" showLibraryParser)

djBuddyOptions :: IO Command
djBuddyOptions =
  execParser $
    info (helper <*> commandParser) $
      (progDesc "Helps you become a big DJ")

handleMakePlaylist :: MakePlaylistOptions -> IO ()
handleMakePlaylist opts =
  do
    lib <- readLibFromFile_ libFilename
    plist <- mkResolvedPlaylist lib opts.firstTrack opts.maxTracks
    putStr $ printResolvedPlaylist plist
  where
    libFilename = case opts.libraryPath of
      Nothing -> "src/library.yaml"
      Just c -> c

-- handleMakePlaylist opts = print opts

runCommand :: Command -> IO ()
runCommand c =
  case c of
    MakePlaylist o -> handleMakePlaylist o
    SuggestNextTracks _ -> undefined
    ShowLibrary _ -> undefined

main :: IO ()
main = do
  selectedCommand <- djBuddyOptions
  runCommand selectedCommand
