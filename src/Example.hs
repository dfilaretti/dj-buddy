{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Example where

import Data.Map.Strict qualified as Map
import MyLib

printLib :: IO ()
printLib = printTrackCatalog library.trackCatalog

nextTracks :: TrackId -> IO ()
nextTracks tid = putStr $ printStuff library tid

playlist :: TrackId -> Int -> IO ()
playlist tid n = do
  p <- mkPlayList library tid n
  putStr (printPlaylist library p)

-----------------------------------------
----- Example data ----------------------
-----------------------------------------

-- Built-in data
another_summer_id = 0

how_could_i_say_it_id = 1

nothing_is_impossible_id = 2

spirit_id = 3

mahon_id = 4

howl_at_the_moon_id = 5

melody_express_id = 6

tribe_of_the_sun_id = 7

inferno_id = 8

against_the_wind_id :: TrackId
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

dan_stone_feel_so_good_id = 22

doubt_and_fear_id = 23

okinawa_id = 24

united_in_love_id = 25

ode_to_autumn_id = 26

breath_easy_id = 27

without_u_id = 28

lost_dimension = 29

library :: MusicLibrary
library =
  MusicLibrary
    { nextTrackId = 100,
      trackCatalog =
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
            (colossal_id, Track "Colossal" (read "4M")),
            (aniara_id, Track "Aniara" (Key K_9 D)),
            (marulio_id, Track "Marulio" (Key K_1 D)),
            (sorrento_id, Track "Sorrento" (Key K_9 M)),
            -- batch 2
            (dan_stone_feel_so_good_id, Track "Feel So Good (Dan Stone)" (Key K_2 M)),
            (doubt_and_fear_id, Track "Doubt And Fear" (Key K_8 D)),
            (okinawa_id, Track "Okinawa" (Key K_11 D)),
            (united_in_love_id, Track "United In Love" (Key K_12 D)),
            (ode_to_autumn_id, Track "Ode to Autumn" (Key K_8 M)),
            (breath_easy_id, Track "Breath Easy" (Key K_11 M)),
            (without_u_id, Track "Without U (Steve RMX)" (Key K_2 D)),
            (lost_dimension, Track "Lost Dimnsion" (Key K_2 M))
          ],
      transitionGraph =
        Map.fromList
          [ ( melody_express_id,
              [ TestedTransition
                  { to = another_summer_id,
                    notes = Nothing
                  }
              ]
            ),
            ( thunderfunk_id,
              [ TestedTransition
                  { to = howl_at_the_moon_id,
                    notes = Nothing
                  }
              ]
            ),
            ( howl_at_the_moon_id,
              [ TestedTransition
                  { to = colossal_id,
                    notes = Nothing
                  }
              ]
            ),
            ( nothing_is_impossible_id,
              [ TestedTransition
                  { to = mahon_id,
                    notes = Nothing
                  },
                TestedTransition
                  { to = falling_id,
                    notes = Nothing
                  }
              ]
            ),
            ( aniara_id,
              [ TestedTransition
                  { to = how_could_i_say_it_id,
                    notes = Nothing
                  }
              ]
            ),
            ( feels_so_good_id,
              [ TestedTransition
                  { to = polkatorju_id,
                    notes = Nothing
                  }
              ]
            ),
            ( polkatorju_id,
              [ TestedTransition
                  { to = resilience_id,
                    notes = Nothing
                  }
              ]
            ),
            ( set_me_free_id,
              [ TestedTransition
                  { to = spirit_id,
                    notes = Nothing
                  },
                TestedTransition
                  { to = sorrento_id,
                    notes = Nothing
                  }
              ]
            ),
            ( spirit_id,
              [ TestedTransition
                  { to = aniara_id,
                    notes = Nothing
                  }
              ]
            ),
            ( colossal_id,
              [ TestedTransition
                  { to = inferno_id,
                    notes = Nothing
                  }
              ]
            ),
            ( resilience_id,
              [ TestedTransition
                  { to = falling_id,
                    notes = Just "mashup: play falling vox over resilience!"
                  }
              ]
            ),
            ( mahon_id,
              [ TestedTransition
                  { to = spirit_id,
                    notes = Nothing
                  },
                TestedTransition
                  { to = feels_so_good_id,
                    notes = Nothing
                  },
                TestedTransition
                  { to = ode_to_autumn_id,
                    notes = Nothing
                  }
              ]
            ),
            ( against_the_wind_id,
              [ TestedTransition
                  { to = feels_so_good_id,
                    notes = Nothing
                  }
              ]
            ),
            ( sorrento_id,
              [ TestedTransition
                  { to = set_me_free_id,
                    notes = Nothing
                  },
                TestedTransition
                  { to = spirit_id,
                    notes = Nothing
                  }
              ]
            ),
            ( marulio_id,
              [ TestedTransition
                  { to = melody_express_id,
                    notes = Nothing
                  },
                TestedTransition
                  { to = without_u_id,
                    notes = Nothing
                  }
              ]
            ),
            ( falling_id,
              [ TestedTransition
                  { to = mahon_id,
                    notes = Nothing
                  },
                TestedTransition
                  { to = marulio_id,
                    notes = Nothing
                  }
              ]
            ),
            ( lost_dimension,
              [ TestedTransition
                  { to = without_u_id,
                    notes = Nothing
                  },
                TestedTransition
                  { to = colossal_id,
                    notes = Nothing
                  }
              ]
            ),
            ( without_u_id,
              [ TestedTransition
                  { to = aeterna_id,
                    notes = Nothing
                  }
              ]
            ),
            ( okinawa_id,
              [ TestedTransition
                  { to = against_the_wind_id,
                    notes = Nothing
                  }
              ]
            )
          ]
    }
