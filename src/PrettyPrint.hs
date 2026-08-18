module PrettyPrint
  (
  )
where

import MusicLibrary (MusicLibrary)
import NextTransition (Transition (..))
import TrackCatalog (Track (Track), TrackCatalog)

prettyPrintTransition :: TrackCatalog -> Transition -> String
prettyPrintTransition = undefined

prettyPrintTransitions :: TrackCatalog -> [Transition] -> String
prettyPrintTransitions = undefined

prettyPrintTrackCatalog :: TrackCatalog -> String
prettyPrintTrackCatalog = undefined

prettyPrintTransitionGraph :: MusicLibrary -> String
prettyPrintTransitionGraph = undefined
