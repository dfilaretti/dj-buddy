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
renderShowPage libFile title key playlistLength result =
  mconcat
    [ "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>Music library</title></head><body>",
      "<h1>Music library</h1>",
      "<form method=\"get\" action=\"/show\">",
      "<p><label for=\"title\">Title</label> ",
      input "text" "title" (maybe "" id title),
      "</p><p><label for=\"key\">Key (e.g. 3D)</label> ",
      input "text" "key" (maybe "" id key),
      "</p><p><label for=\"n\">Playlist length (default 10)</label> ",
      input "number" "n" (maybe "" id playlistLength),
      "</p>",
      maybe "" (input "hidden" "lib") libFile,
      " <button type=\"submit\">Apply filters</button></form>",
      case result of
        Left message -> "<p role=\"alert\">" <> escape message <> "</p>"
        Right [] -> "<p>No tracks found.</p>"
        Right tracks -> "<ul>" <> mconcat (map (renderTrack libFile playlistLength) tracks) <> "</ul>",
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
renderTrack libFile playlistLength (trackId, track) =
  "<li>"
    <> escape (trackId ++ ": " ++ show track)
    <> " <a href=\""
    <> escape (playlistUrl libFile playlistLength trackId)
    <> "\">Generate playlist</a></li>"

playlistUrl :: Maybe String -> Maybe String -> TrackId -> String
playlistUrl libFile playlistLength trackId =
  "/playlist" ++ StrictText.unpack (decodeUtf8 (renderQuery True parameters))
  where
    parameters =
      [("start", Just (encode trackId))]
        ++ [("n", Just (encode n)) | Just n <- [playlistLength], n /= ""]
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
