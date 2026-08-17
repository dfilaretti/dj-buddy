module Lib
  ( library,
  )
where

import Camelot
import qualified Data.Map as Map
import qualified MusicLibrary as M
import Track

library :: M.MusicLibrary
library =
  M.MusicLibrary
    { M.nextTrackId = 100,
      M.trackCatalog =
        Map.fromList
          [ (0, (Track "A" (K_1, A))),
            (1, (Track "A" (K_1, A))),
            (2, (Track "A" (K_1, A))),
            (3, (Track "A" (K_1, A))),
            (4, (Track "A" (K_1, A))),
            (5, (Track "A" (K_1, A))),
            (6, (Track "A" (K_1, A))),
            (7, (Track "A" (K_1, A))),
            (8, (Track "A" (K_1, A))),
            (9, (Track "A" (K_1, A))),
            (10, (Track "A" (K_1, A)))
          ],
      M.transitionGraph =
        Map.fromList
          [ (0, []),
            (1, []),
            (2, [])
          ]
    }
