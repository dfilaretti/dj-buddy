module Web.ShowPage (renderShowPage) where

import Data.Text.Lazy (Text)
import Domain.MusicLibrary (Track, TrackId)
import Web.Html (page, escape, queryUrl)

renderShowPage ::
  Maybe String ->
  Maybe String ->
  Maybe String ->
  Maybe String ->
  Either String [(TrackId, Track)] ->
  Text
renderShowPage libFile title key resultCount result =
  page "Music library" $ mconcat
    [ "<form method=\"get\" action=\"/show\">",
      "<p><label for=\"title\">Title</label> ",
      input "text" "title" (maybe "" id title),
      "</p><p><label for=\"key\">Key (e.g. 3D)</label> ",
      input "text" "key" (maybe "" id key),
      "</p><p><label for=\"n\">Number of tracks (defaults: playlist 10, suggestions 3)</label> ",
      input "number" "n" (maybe "" id resultCount),
      "</p>",
      maybe "" (input "hidden" "lib") libFile,
      " <button type=\"submit\">Apply filters</button>",
      " <button type=\"submit\" formaction=\"/playlist\">Random playlist</button></form>",
      case result of
        Left message -> "<p role=\"alert\">" <> escape message <> "</p>"
        Right [] -> "<p>No tracks found.</p>"
        Right tracks -> "<ul>" <> mconcat (map (renderTrack libFile resultCount) tracks) <> "</ul>"
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
  queryUrl route parameters
  where
    parameters =
      [(trackParam, trackId)]
        ++ [("n", n) | Just n <- [resultCount], n /= ""]
        ++ [("lib", path) | Just path <- [libFile]]
