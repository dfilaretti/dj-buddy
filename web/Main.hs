import App (getTracks, getTransitions, mkPlaylist, readLibFromFile_)
import Data.Text qualified as StrictText
import Data.Text.Lazy qualified as Text
import Domain.Camelot (Key)
import Network.HTTP.Types.Status (badRequest400, internalServerError500)
import Text.Read (readEither)
import Web.Scotty hiding (readEither)
import Web.ShowPage (renderShowPage)

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
    let render = html . renderShowPage libFile title rawKey
    key <- case traverse (readEither :: String -> Either String Key) (nonEmpty rawKey) of
      Right parsedKey -> pure parsedKey
      Left _ -> do
        status badRequest400
        render $ Left "Invalid key. Expected a number from 1 to 12 followed by D or M, for example 3D or 4M."
        finish
    result <- liftIO $ readLibFromFile_ (maybe defaultLibFile id libFile)
    case result of
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
    start <- queryParamMaybe "start" :: ActionM (Maybe String)
    rawN <- queryParamMaybe "n"
    n <- case traverse (readEither :: String -> Either String Int) rawN of
      Right parsedN -> pure $ maybe 10 id parsedN
      Left _ -> do
        status badRequest400
        text "Invalid n. Expected an integer."
        finish
    musicLibE <- liftIO $ readLibFromFile_ (maybe defaultLibFile id libFile)
    case musicLibE of
      Left err -> do
        status internalServerError500
        text $ Text.pack $ show err
      Right lib -> do
        playlistE <- liftIO $ mkPlaylist lib start n
        case playlistE of
          Left err -> do
            status internalServerError500
            text $ Text.pack $ show err
          Right playlist ->
            text $ Text.pack $ unlines $ map show playlist

suggestHandler :: ScottyM ()
suggestHandler =
  get "/suggest" $ do
    libFile <- queryParamMaybe "lib"
    rawTrack <- queryParamMaybe "track"
    track <- case rawTrack of
      Just trackId -> pure trackId
      Nothing -> do
        status badRequest400
        text "Missing track. Provide a track ID with ?track=TRACK_ID."
        finish
    rawN <- queryParamMaybe "n"
    n <- case traverse (readEither :: String -> Either String Int) rawN of
      Right parsedN -> pure $ maybe 3 id parsedN
      Left _ -> do
        status badRequest400
        text "Invalid n. Expected an integer."
        finish
    musicLibE <- liftIO $ readLibFromFile_ (maybe defaultLibFile id libFile)
    case musicLibE of
      Left err -> do
        status internalServerError500
        text $ Text.pack $ show err
      Right lib ->
        case getTransitions lib track n of
          Left err -> do
            status badRequest400
            text $ Text.pack $ show err
          Right transitions ->
            text $ Text.pack $ unlines $ map show transitions

defaultHandler_ :: ScottyM ()
defaultHandler_ =
  get "/:word" $ do
    beam <- pathParam "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
