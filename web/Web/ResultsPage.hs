module Web.ResultsPage (renderPlaylistPage, renderSuggestPage) where

import App (ResolvedPlaylist, ResolvedPlaylistMember (..), ResolvedTransition (..))
import Data.Text.Lazy (Text)
import Domain.MusicLibrary (TestedTransition (..))
import Domain.Playlist (PlaylistMember (..), getTrackId_)
import Domain.Transitions (HarmonicTransType (..), Transition (..), destTrackId)
import Web.Html (escape, page, queryUrl)

renderPlaylistPage :: Maybe String -> Either String ResolvedPlaylist -> Text
renderPlaylistPage libFile result =
  renderResultsPage "Playlist" libFile " <a href=\"\">Refresh</a>" $ fmap (map formatEntry) result
  where
    formatEntry entry =
      getTrackId_ entry.playlistMember
        ++ ": "
        ++ show entry.track
        ++ " — "
        ++ playlistMemberLabel entry.playlistMember

renderSuggestPage :: Maybe String -> Either String [ResolvedTransition] -> Text
renderSuggestPage libFile result =
  renderResultsPage "Compatible tracks" libFile "" $ fmap (map formatEntry) result
  where
    formatEntry entry =
      destTrackId entry.transition
        ++ ": "
        ++ show entry.track
        ++ " — "
        ++ transitionLabel entry.transition

playlistMemberLabel :: PlaylistMember -> String
playlistMemberLabel (StartingTrack _) = "Starting track"
playlistMemberLabel (MiddleTrack transition) = transitionLabel transition

transitionLabel :: Transition -> String
transitionLabel (Tested info _) =
  "Tested transition" ++ maybe "" (": " ++) info.notes
transitionLabel (Harmonic SameKey _) = "Same key"
transitionLabel (Harmonic CompatibleKeys _) = "Compatible key"

renderResultsPage :: String -> Maybe String -> Text -> Either String [String] -> Text
renderResultsPage title libFile extraLinks result =
  page title $
    "<p><a href=\""
      <> escape libraryUrl
      <> "\">Back to library</a>"
      <> extraLinks
      <> "</p>"
      <> case result of
        Left message -> "<p role=\"alert\">" <> escape message <> "</p>"
        Right [] -> "<p>No tracks found.</p>"
        Right entries -> "<ol>" <> mconcat (map renderEntry entries) <> "</ol>"
  where
    libraryUrl = queryUrl "/show" [("lib", path) | Just path <- [libFile]]
    renderEntry entry = "<li>" <> escape entry <> "</li>"
