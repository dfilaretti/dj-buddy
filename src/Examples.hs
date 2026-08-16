module Examples
  ( library,
  )
where

import Graph (Edge (..))
import Key
import qualified MusicLibrary as M
import Track

tracks :: [Track]
tracks =
  [ (Track "X" (K_1, A)),
    (Track "Y" (K_1, A)),
    (Track "X" (K_1, A)),
    (Track "X" (K_1, A))
  ]

transitions :: [(TrackId, Edge)]
transitions = [(0, Edge 2)]

library =
  populate M.empty
  where
    populate = M.addTransitions transitions . M.addTracks tracks
