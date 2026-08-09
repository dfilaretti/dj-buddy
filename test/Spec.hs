module Main where

import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Key" $ do
        it "compares equal values correctly" $ do
            A 3 `shouldBe` A 3

        it "distinguishes different values" $ do
            A 3 `shouldNotBe` B 3

    describe "Track" $ do
        it "stores its title and key" $ do
            Track "Thunderfunk" (A 3)
                `shouldBe` Track "Thunderfunk" (A 3)

-- stack test --test-arguments "--match Key"
-- stack test
