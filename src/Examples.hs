module Examples
  ( library,
    graph,
  )
where

import qualified Data.Map.Strict as Map
import Key (KeyNumber (..), KeyType (..))
import Lib

library :: Library
library =
  Map.fromList
    [ (1, Track "A" (K_3, A)),
      (3, Track "C" (K_6, A)),
      (4, Track "D" (K_6, A)),
      (2, Track "B" (K_3, B)),
      (5, Track "E" (K_3, B)),
      (6, Track "F" (K_3, B))
    ]

graph :: Graph
graph =
  Map.fromList
    [ (5, [Edge 6])
    ]
