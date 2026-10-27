import App (getTracks, getTransitions, mkPlaylist, readLibFromFile_)
import Data.Text qualified as StrictText
import Domain.Camelot (Key)
import Network.HTTP.Types.Status (badRequest400, internalServerError500)
import Text.Read (readEither)
import Web.Scotty hiding (readEither)
import Web.ShowPage (renderShowPage)
import Web.ResultsPage (renderPlaylistPage, renderSuggestPage)

main :: IO ()
main = scotty 3000 $ do
  showHandler
  playlistHandler
  suggestHandler
  defaultHandler_

defaultLibFile :: String
defaultLibFile = "src/library.yaml"

showHandler :: ScottyM ()
showHandler =
  get "/show" $ do
    libFile <- queryParamMaybe "lib"
    title <- queryParamMaybe "title"
    rawKey <- queryParamMaybe "key"
    rawN <- queryParamMaybe "n"
    let render = html . renderShowPage libFile title rawKey rawN
    case traverse (readEither :: String -> Either String Int) (nonEmpty rawN) of
      Right _ -> pure ()
      Left _ -> do
        status badRequest400
        render $ Left "Invalid number of tracks. Expected an integer."
        finish
    key <- case traverse (readEither :: String -> Either String Key) (nonEmpty rawKey) of
      Right parsedKey -> pure parsedKey
      Left _ -> do
        status badRequest400
        render $ Left "Invalid key. Expected a number from 1 to 12 followed by D or M, for example 3D or 4M."
        finish
    musicLibraryE <- liftIO $ readLibFromFile_ (maybe defaultLibFile id libFile)
    case musicLibraryE of
      Left err -> do
        status internalServerError500
        render $ Left $ show err
      Right lib ->
        render $ Right $ getTracks lib (StrictText.pack <$> nonEmpty title) key

nonEmpty :: Maybe String -> Maybe String
nonEmpty (Just "") = Nothing
nonEmpty value = value

playlistHandler :: ScottyM ()
playlistHandler =
  get "/playlist" $ do
    libFile <- queryParamMaybe "lib"
    let render = html . renderPlaylistPage libFile
    start <- queryParamMaybe "start" :: ActionM (Maybe String)
    rawN <- queryParamMaybe "n"
    n <- case traverse (readEither :: String -> Either String Int) rawN of
      Right parsedN -> pure $ maybe 10 id parsedN
      Left _ -> do
        status badRequest400
        render $ Left "Invalid n. Expected an integer."
        finish
    musicLibE <- liftIO $ readLibFromFile_ (maybe defaultLibFile id libFile)
    case musicLibE of
      Left err -> do
        status internalServerError500
        render $ Left $ show err
      Right lib -> do
        playlistE <- liftIO $ mkPlaylist lib start n
        case playlistE of
          Left err -> do
            status internalServerError500
            render $ Left $ show err
          Right playlist ->
            render $ Right playlist

suggestHandler :: ScottyM ()
suggestHandler =
  get "/suggest" $ do
    libFile <- queryParamMaybe "lib"
    let render = html . renderSuggestPage libFile
    rawTrack <- queryParamMaybe "track"
    track <- case rawTrack of
      Just trackId -> pure trackId
      Nothing -> do
        status badRequest400
        render $ Left "Missing track. Provide a track ID with ?track=TRACK_ID."
        finish
    rawN <- queryParamMaybe "n"
    n <- case traverse (readEither :: String -> Either String Int) rawN of
      Right parsedN -> pure $ maybe 3 id parsedN
      Left _ -> do
        status badRequest400
        render $ Left "Invalid n. Expected an integer."
        finish
    musicLibE <- liftIO $ readLibFromFile_ (maybe defaultLibFile id libFile)
    case musicLibE of
      Left err -> do
        status internalServerError500
        render $ Left $ show err
      Right lib ->
        case getTransitions lib track n of
          Left err -> do
            status badRequest400
            render $ Left $ show err
          Right transitions ->
            render $ Right transitions

defaultHandler_ :: ScottyM ()
defaultHandler_ =
  get "/:word" $ do
    beam <- pathParam "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
