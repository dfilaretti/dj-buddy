module Main (main) where

import Lib
import System.Exit (exitFailure)
import Test.QuickCheck (Result, isSuccess, quickCheckResult)

-----------------------------------------
----- Track Library ---------------------
-----------------------------------------

prop_emptyTrackCatalog :: TrackId -> Bool
prop_emptyTrackCatalog tid =
  getTrackWithId tid emptyCatalog == Nothing

prop_trackInTrackOut :: TrackId -> Bool
prop_trackInTrackOut tid =
  getTrackWithId tid catalog == Just track
  where
    catalog = addTrack tid track emptyCatalog
    track = Track "Test Track" (Key K_3 D)

-----------------------------------------
----- Test Runner (main) ----------------
-----------------------------------------

main :: IO ()
main = do
  _ <- quickCheckResult prop_emptyTrackCatalog
  _ <- quickCheckResult prop_trackInTrackOut
  return ()
