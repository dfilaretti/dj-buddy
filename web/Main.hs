import App (getTracks, readLibFromFile_)
import Data.Text.Lazy qualified as Text
import Domain.Camelot (Key)
import Network.HTTP.Types.Status (badRequest400, internalServerError500)
import Text.Read (readEither)
import Web.Scotty hiding (readEither)

main :: IO ()
main = scotty 3000 $ do
  showHandler
  defaultHandler_

defaultLibFile :: String
defaultLibFile = "src/library.yaml"

showHandler :: ScottyM ()
showHandler =
  get "/show" $ do
    libFile <- queryParamMaybe "lib"
    title <- queryParamMaybe "title"
    rawKey <- queryParamMaybe "key"
    key <- case traverse (readEither :: String -> Either String Key) rawKey of
      Right parsedKey -> pure parsedKey
      Left _ -> do
        status badRequest400
        text "Invalid key. Expected a number from 1 to 12 followed by D or M, for example 3D or 4M."
        finish
    result <- liftIO $ readLibFromFile_ (maybe defaultLibFile id libFile)
    case result of
      Left err -> do
        status internalServerError500
        text $ Text.pack $ show err
      Right lib ->
        text $
          Text.pack $
            unlines
              [ trackId ++ ": " ++ show track
              | (trackId, track) <- getTracks lib title key
              ]

defaultHandler_ :: ScottyM ()
defaultHandler_ =
  get "/:word" $ do
    beam <- pathParam "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
