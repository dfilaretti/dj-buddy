{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Lib
import System.Exit (exitFailure)
import Test.QuickCheck (Arbitrary (arbitrary), Result, elements, isSuccess, quickCheckResult)

instance Arbitrary Track where
  arbitrary =
    Track
      <$> arbitrary
      <*> ( Key
              <$> elements [minBound .. maxBound]
              <*> elements [D, M]
          )

-----------------------------------------
----- Track Library ---------------------
-----------------------------------------

prop_emptyTrackCatalog :: TrackId -> Bool
prop_emptyTrackCatalog tid =
  getTrackWithId tid emptyCatalog == Nothing

prop_trackInTrackOut :: TrackId -> Track -> Bool
prop_trackInTrackOut tid track =
  getTrackWithId tid catalog == Just track
  where
    catalog = addTrack tid track emptyCatalog

prop_testedTransitionWins :: TrackId -> TrackId -> Track -> Bool
prop_testedTransitionWins tid1 tid2 t1 =
  transitions lib tid1
    == [ Tested $
           TestedTransition
             { to = tid2,
               rating = Omg,
               notes = Nothing
             }
       ]
  where
    lib =
      MusicLibrary
        { transitionGraph =
            Map.fromList
              [(tid1, [TestedTransition tid2 Omg Nothing])],
          trackCatalog =
            Map.fromList
              [ (tid1, t1),
                (tid2, Track "B" (t1.key)) -- this will produce a "same key" transition
              ],
          nextTrackId = 0
        }

-----------------------------------------
----- Test Runner (main) ----------------
-----------------------------------------

tests =
  [ quickCheckResult prop_emptyTrackCatalog,
    quickCheckResult prop_trackInTrackOut,
    quickCheckResult prop_testedTransitionWins
  ]

main :: IO ()
main = sequence_ tests
