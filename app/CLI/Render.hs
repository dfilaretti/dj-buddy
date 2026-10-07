module CLI.Render where

import App (ResolvedPlaylist, ResolvedPlaylistMember (..), ResolvedTransition (..))
import Data.Map.Strict qualified as Map
import Domain.Playlist (PlaylistMember (..))
import Domain.TestedTransitions (TestedTransition (..))
import Domain.TrackCatalog (TrackCatalog)
import Domain.Transitions (Transition (..))

renderTrackCatalog :: TrackCatalog -> String
renderTrackCatalog =
  unlines . map formatTrack . Map.toList
  where
    formatTrack (trackId, track) =
      show trackId ++ ": " ++ show track

renderTransition :: Transition -> String
renderTransition (Tested i) = "Tested (notes: " ++ notes ++ ")"
  where
    notes = case i.notes of
      Nothing -> "NA"
      Just msg -> msg
renderTransition (Harmonic t _) = "Harmonic: " ++ show t

renderPlaylistMember :: PlaylistMember -> String
renderPlaylistMember (StartingTrack _) = "start"
renderPlaylistMember (MiddleTrack t) = renderTransition t

renderResolvedPlaylistMember :: ResolvedPlaylistMember -> String
renderResolvedPlaylistMember pm =
  show (pm.track) ++ " - " ++ renderPlaylistMember (pm.playlistMember)

renderResolvedPlaylist :: ResolvedPlaylist -> String
renderResolvedPlaylist =
  unlines . map renderResolvedPlaylistMember

renderResolvedTransition :: ResolvedTransition -> String
renderResolvedTransition rt = show rt.track ++ " (" ++ renderTransition rt.transition ++ ")"

renderResolvedTransitions :: [ResolvedTransition] -> String
renderResolvedTransitions = unlines . map renderResolvedTransition
