module Web.ShowPage (renderShowPage) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Domain.MusicLibrary (Track, TrackId)

renderShowPage ::
  Maybe String ->
  Maybe String ->
  Maybe String ->
  Either String [(TrackId, Track)] ->
  Text
renderShowPage libFile title key result =
  mconcat
    [ "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>Music library</title></head><body>",
      "<h1>Music library</h1>",
      "<form method=\"get\" action=\"/show\">",
      "<p><label for=\"title\">Title</label> ",
      input "text" "title" (maybe "" id title),
      "</p><p><label for=\"key\">Key (e.g. 3D)</label> ",
      input "text" "key" (maybe "" id key),
      "</p>",
      maybe "" (input "hidden" "lib") libFile,
      " <button type=\"submit\">Apply filters</button></form>",
      case result of
        Left message -> "<p role=\"alert\">" <> escape message <> "</p>"
        Right [] -> "<p>No tracks found.</p>"
        Right tracks -> "<ul>" <> mconcat (map renderTrack tracks) <> "</ul>",
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

renderTrack :: (TrackId, Track) -> Text
renderTrack (trackId, track) =
  "<li>" <> escape (trackId ++ ": " ++ show track) <> "</li>"

escape :: String -> Text
escape = Text.pack . concatMap escapeChar
  where
    escapeChar '&' = "&amp;"
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&#39;"
    escapeChar c = [c]
