module Lib
  ( library,
  )
where

import Camelot
import qualified MusicLibrary as M
import Track
import TransitionsGraph (Edge (..))

tracks :: [Track]
tracks =
  [ (Track "A" (K_1, A)),
    (Track "B" (K_1, A)),
    (Track "C" (K_1, B)),
    (Track "D" (K_1, A))
  ]

transitions :: [(TrackId, Edge)]
transitions = [(0, Edge 2)]

library :: M.MusicLibrary
library = populate M.empty
  where
    populate = M.addTransitions transitions . M.addTracks tracks
