module Main (main) where

import Camelot (KeyNumber (..), KeyType (..))
import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "trackKey" $ do
        it "gives Nothing if library is empty" $ do
            trackKey 0 emptyLibrary
                `shouldBe` Nothing

        it "... or if track is not found" $ do
            trackKey 123 (addToLibrary 0 (Track "foo" (K_1, A)) emptyLibrary)
                `shouldBe` Nothing

        it "gives you the key if track is found" $ do
            trackKey 0 (addToLibrary 0 (Track "foo" (K_1, A)) emptyLibrary)
                `shouldBe` Just (K_1, A)



-- stack test --test-arguments "--match Key"
-- stack test
