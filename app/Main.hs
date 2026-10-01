module Main (main) where

import CLI.Render
import Domain.Camelot (Key)
import Domain.TrackCatalog (TrackId)
import Options.Applicative

data MakePlaylistOptions = MakePlaylistOptions
  { firstTrack :: Maybe TrackId,
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

playlistParser :: ParserInfo Command
playlistParser =
  info
    (pure $ MakePlaylist (MakePlaylistOptions Nothing (Just 10)))
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
    (command "playlist" playlistParser)
      <> (command "suggest" suggestParser)
      <> (command "show" showLibraryParser)

djBuddyOptions :: IO Command
djBuddyOptions =
  execParser $
    info (helper <*> commandParser) $
      (progDesc "Helps you become a big DJ")

handleMakePlaylist :: MakePlaylistOptions -> IO ()
handleMakePlaylist _ = showRandomPlaylist 10

-- handleMakePlaylist opts = print opts

main :: IO ()
main = do
  selectedCommand <- djBuddyOptions
  case selectedCommand of
    MakePlaylist o -> handleMakePlaylist o
    SuggestNextTracks _ -> undefined
    ShowLibrary _ -> undefined
