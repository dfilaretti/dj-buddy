module Web.Html (page, escape, queryUrl) where

import Data.Text qualified as StrictText
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Network.HTTP.Types.URI (renderQuery)

page :: String -> Text -> Text
page title body =
  "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>"
    <> escape title
    <> "</title><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    <> "<link rel=\"stylesheet\" href=\"/static/style.css\"></head><body><main><h1>"
    <> escape title
    <> "</h1>"
    <> body
    <> "</main></body></html>"

queryUrl :: String -> [(String, String)] -> String
queryUrl route parameters =
  route ++ StrictText.unpack (decodeUtf8 (renderQuery True encoded))
  where
    encoded = [(encode key, Just (encode value)) | (key, value) <- parameters]
    encode = encodeUtf8 . StrictText.pack

escape :: String -> Text
escape = Text.pack . concatMap escapeChar
  where
    escapeChar '&' = "&amp;"
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&#39;"
    escapeChar c = [c]
