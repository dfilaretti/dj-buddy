{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lib
  ( library,
  )
where

import Camelot
import qualified Data.Map as Map
import qualified MusicLibrary as M
import Track
import TransitionsGraph (Edge (..))

another_summer_id = 0

how_could_i_say_it_id = 1

nothing_is_impossible_id = 2

spirit_id = 3

mahon_id = 4

howl_at_the_moon_id = 5

melody_express_id = 6

tribe_of_the_sun_id = 7

inferno_id = 8

against_the_wind_id = 9

resilience_id = 10

too_much_rain_id = 11

aeterna_id = 12

set_me_free_id = 13

thunderfunk_id = 14

polkatorju_id = 15

falling_id = 16

feels_so_good_id = 17

colossal_id = 18

aniara_id = 19

marulio_id = 20

sorrento_id = 21

library :: M.MusicLibrary
library =
  M.MusicLibrary
    { M.nextTrackId = 100,
      M.trackCatalog =
        Map.fromList
          [ (another_summer_id, Track "Another Summer" (Key K_3 D)),
            (how_could_i_say_it_id, Track "How Could I say It" (Key K_10 D)),
            (nothing_is_impossible_id, Track "Nothing Is Impossible" (Key K_7 D)),
            (spirit_id, Track "Spirit" (Key K_9 D)),
            (mahon_id, Track "Mahon" (Key K_8 D)),
            (howl_at_the_moon_id, Track "Howl At The Moon" (Key K_1 M)),
            (melody_express_id, Track "Melody Express" (Key K_3 D)),
            (tribe_of_the_sun_id, Track "Tribe Of The Sun" (Key K_4 D)),
            (inferno_id, Track "Inferno" (Key K_4 D)),
            (against_the_wind_id, Track "Against The Wind" (Key K_10 D)),
            (resilience_id, Track "Resilience" (Key K_5 D)),
            (too_much_rain_id, Track "Too Much Rain" (Key K_1 D)),
            (aeterna_id, Track "Aeterna" (Key K_2 D)),
            (set_me_free_id, Track "Set Me Free" (Key K_9 M)),
            (thunderfunk_id, Track "Thunderfunk" (Key K_1 M)),
            (polkatorju_id, Track "Polkatorju" (Key K_10 D)),
            (falling_id, Track "Falling" (Key K_6 D)),
            (feels_so_good_id, Track "Feels So Good" (Key K_10 D)),
            (colossal_id, Track "Colossal" (Key K_4 M)),
            (aniara_id, Track "Aniara" (Key K_9 D)),
            (marulio_id, Track "Marulio" (Key K_1 D)),
            (sorrento_id, Track "Sorrento" (Key K_9 M))
          ],
      M.transitionGraph =
        Map.fromList
          [ -- melody espress => Another Summer
            ( melody_express_id,
              [ Edge another_summer_id
              ]
            ),
            -- thunderfunk => howl at the moon
            ( thunderfunk_id,
              [Edge howl_at_the_moon_id]
            ),
            -- howl at the moon => colossal
            ( howl_at_the_moon_id,
              [Edge colossal_id]
            ),
            -- nopthing is impossible
            --     => mahon
            --     => falling
            ( nothing_is_impossible_id,
              [ Edge mahon_id,
                Edge falling_id
              ]
            ),
            ( aniara_id,
              [Edge how_could_i_say_it_id]
            ),
            ( feels_so_good_id,
              [Edge polkatorju_id]
            ),
            ( set_me_free_id,
              [Edge spirit_id, Edge sorrento_id]
            ),
            ( spirit_id,
              [Edge aniara_id]
            ),
            ( colossal_id,
              [Edge inferno_id]
            ),
            ( resilience_id,
              [Edge falling_id] -- Mashup
            ),
            ( mahon_id,
              [ Edge spirit_id,
                Edge feels_so_good_id -- +2 jump to Feels So Good
              ]
            ),
            ( against_the_wind_id,
              [Edge feels_so_good_id]
            ),
            ( sorrento_id,
              [ Edge set_me_free_id,
                Edge spirit_id
              ]
            ),
            -- Not so great
            ( marulio_id,
              [Edge melody_express_id]
            ),
            ( falling_id,
              [Edge mahon_id]
            )
          ]
    }
