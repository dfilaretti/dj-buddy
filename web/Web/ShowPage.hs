module Web.ShowPage (renderShowPage) where

import Data.Text qualified as StrictText
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Domain.MusicLibrary (Track, TrackId)
import Network.HTTP.Types.URI (renderQuery)

renderShowPage ::
  Maybe String ->
  Maybe String ->
  Maybe String ->
  Maybe String ->
  Either String [(TrackId, Track)] ->
  Text
renderShowPage libFile title key resultCount result =
  mconcat
    [ "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>Music library</title></head><body>",
      "<h1>Music library</h1>",
      "<form method=\"get\" action=\"/show\">",
      "<p><label for=\"title\">Title</label> ",
      input "text" "title" (maybe "" id title),
      "</p><p><label for=\"key\">Key (e.g. 3D)</label> ",
      input "text" "key" (maybe "" id key),
      "</p><p><label for=\"n\">Number of tracks (defaults: playlist 10, suggestions 3)</label> ",
      input "number" "n" (maybe "" id resultCount),
      "</p>",
      maybe "" (input "hidden" "lib") libFile,
      " <button type=\"submit\">Apply filters</button></form>",
      case result of
        Left message -> "<p role=\"alert\">" <> escape message <> "</p>"
        Right [] -> "<p>No tracks found.</p>"
        Right tracks -> "<ul>" <> mconcat (map (renderTrack libFile resultCount) tracks) <> "</ul>",
      "</body></html>"
    ]

input :: Text -> Text -> String -> Text
input inputType name value =
  "<input type=\""
    <> inputType
    <> "\" id=\""
    <> name
    <> "\" name=\""
    <> name
    <> "\" value=\""
    <> escape value
    <> "\">"

renderTrack :: Maybe String -> Maybe String -> (TrackId, Track) -> Text
renderTrack libFile resultCount (trackId, track) =
  "<li>"
    <> escape (trackId ++ ": " ++ show track)
    <> " <a href=\""
    <> escape (trackUrl "/playlist" "start" libFile resultCount trackId)
    <> "\">Generate playlist</a> <a href=\""
    <> escape (trackUrl "/suggest" "track" libFile resultCount trackId)
    <> "\">Suggest compatible</a></li>"

trackUrl :: String -> String -> Maybe String -> Maybe String -> TrackId -> String
trackUrl route trackParam libFile resultCount trackId =
  route ++ StrictText.unpack (decodeUtf8 (renderQuery True parameters))
  where
    parameters =
      [(encode trackParam, Just (encode trackId))]
        ++ [("n", Just (encode n)) | Just n <- [resultCount], n /= ""]
        ++ [("lib", Just (encode path)) | Just path <- [libFile]]
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
