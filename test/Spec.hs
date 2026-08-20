module Main (main) where

import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "track catalog" $ do
    it "gives Nothing when a track is not found" $ do
      getTrackWithId 123 emptyCatalog `shouldBe` Nothing

    it "returns a track that was added" $ do
      let track = Track "foo" (Key K_1 D)
      getTrackWithId 0 (addTrack 0 track emptyCatalog) `shouldBe` Just track

    it "finds other tracks with the same key" $ do
      let catalog = addTrack 1 (Track "bar" (Key K_1 D))
                  $ addTrack 0 (Track "foo" (Key K_1 D)) emptyCatalog
      sameKeySuccessors 0 catalog `shouldBe` [1]
