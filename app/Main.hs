module Main (main) where

import App
  ( getTracks,
    getTransitions,
    mkPlaylist,
    readLibFromFile_,
  )
import CLI.Render
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Text (Text)
import Domain.Camelot (Key)
import Domain.MusicLibrary (TrackId)
import Options.Applicative

-- TODO: move those helpers somewhere shared
catchExceptT :: (Monad m) => ExceptT e m a -> (e -> m a) -> m a
catchExceptT m hdl = runExceptT m >>= either hdl pure

handleExceptT :: (Monad m) => (e -> m a) -> ExceptT e m a -> m a
handleExceptT = flip catchExceptT

handleExceptTFail :: (Monad m, MonadFail m, Show e) => ExceptT e m a -> m a
handleExceptTFail x =
  handleExceptT (fail . show) x

-----------------------------------------------------------

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

-- TODO: all those subcommands share the "lib" option; c
-- consider factoring it out instead of repeating it inside each sub parser
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
                  ( strOption
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

defaultLibraryPath :: String
defaultLibraryPath = "src/library.yaml"

handleMakePlaylist :: MakePlaylistOptions -> IO ()
handleMakePlaylist opts =
  handleExceptTFail do
    lib <- ExceptT $ readLibFromFile_ opts.libraryPath
    plist <- ExceptT $ mkPlaylist lib opts.firstTrack opts.maxTracks
    liftIO $ putStr $ renderResolvedPlaylist plist

handleShowLibrary :: ShowLibraryOptions -> IO ()
handleShowLibrary o =
  handleExceptTFail do
    lib <- ExceptT $ readLibFromFile_ o.libraryPath
    ts <- pure $ getTracks lib o.title o.key
    liftIO $ putStr $ renderMusicLibrary ts

handleSuggestNextTracks :: SuggestNextTracksOptions -> IO ()
handleSuggestNextTracks o =
  handleExceptTFail do
    lib <- ExceptT $ readLibFromFile_ o.libraryPath
    ts <- ExceptT $ pure $ getTransitions lib o.track o.maxTracks
    liftIO $ putStr $ renderResolvedTransitions ts

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
