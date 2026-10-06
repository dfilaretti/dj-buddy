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

printTransitionType :: Transition -> String
printTransitionType (Tested i) = "Tested (notes: " ++ notes ++ ")"
  where
    notes = case i.notes of
      Nothing -> "NA"
      Just msg -> msg
printTransitionType (Harmonic t _) = "Harmonic: " ++ show t

printPlaylistMember :: PlaylistMember -> String
printPlaylistMember (StartingTrack _) = "start"
printPlaylistMember (MiddleTrack t) = printTransitionType t

printResolvedPlaylistMember :: ResolvedPlaylistMember -> String
printResolvedPlaylistMember pm =
  show (pm.track) ++ " - " ++ printPlaylistMember (pm.playlistMember)

printResolvedPlaylist :: ResolvedPlaylist -> String
printResolvedPlaylist =
  unlines . map printResolvedPlaylistMember

renderResolvedTransition :: ResolvedTransition -> String
renderResolvedTransition rt = show rt.track ++ " (" ++ printTransitionType rt.transition ++ ")"

renderResolvedTransitions :: [ResolvedTransition] -> String
renderResolvedTransitions = unlines . map renderResolvedTransition
