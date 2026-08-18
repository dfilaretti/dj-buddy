module PrettyPrint
  (
  )
where

import NextTransition (Transition (..))
import TrackCatalog (Track (Track), TrackCatalog)

prettyPrintTransition :: TrackCatalog -> Transition -> String
-- TODO
prettyPrintTransition catalog (Transition tType to from) = undefined
