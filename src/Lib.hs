module Lib
  ( library,
  )
where

import Camelot
import qualified Data.Map as Map
import qualified MusicLibrary as M
import Track
import TransitionsGraph (Edge (..))

library :: M.MusicLibrary
library =
  M.MusicLibrary
    { M.nextTrackId = 100,
      M.trackCatalog =
        Map.fromList
          [ (0, (Track "Another Summer" (K_3, D))),
            (1, (Track "How Could I say It" (K_10, D))),
            (2, (Track "Nothing Is Impossible" (K_7, D))),
            (3, (Track "Spirit" (K_9, D))),
            (4, (Track "Mahon" (K_8, D))),
            (5, (Track "Howl At The Moon" (K_1, M))),
            (6, (Track "Melody Express" (K_3, D))),
            (7, (Track "Tribe Of The Sun" (K_4, D))),
            (8, (Track "Inferno" (K_4, D))),
            (9, (Track "Against The Wind" (K_10, D))),
            (10, (Track "Resilience" (K_5, D))),
            (11, (Track "Too Much Rain" (K_1, D))),
            (12, (Track "Aeterna" (K_2, D))),
            (13, (Track "Set Me Free" (K_9, M))),
            (14, (Track "Thunderfunk" (K_1, M))),
            (15, (Track "Polkatorju" (K_10, D))),
            (16, (Track "Falling" (K_6, D))),
            (17, (Track "Feels So Good" (K_10, D))),
            (18, (Track "Colossal" (K_4, M))),
            (19, (Track "Aniara" (K_9, D))),
            (20, (Track "Marulio" (K_1, D))),
            (21, (Track "Sorrento" (K_9, M)))
          ],
      M.transitionGraph =
        Map.fromList
          [ -- melody espress => Another Summer
            (6, [Edge 0]),
            -- thunderfunk => howl at the moon
            (14, [Edge 5]),
            -- howl at the moon => colossal
            (5, [Edge 18]),
            -- nopthing is impossible
            --     => mahon
            --     => falling
            (2, [Edge 4, Edge 16])
            -- aniara => how could I say it
            -- feels so good => polkatorju
            -- set me free => spirit
            -- spirit => aniara
            -- colossal => inferno
            -- resilience => falling (*** mashup!!)
            -- mahon -> spirit
            -- against the wind -> feels so good
            -- set me free <-> sorrento (bidirectional)
            -- sorrento -> spirit (*)
            -- Mahon -> Feels so good (+2 jump!)
            -- Marulio => melody express (not so great)
            -- Falling => mahon
          ]
    }
