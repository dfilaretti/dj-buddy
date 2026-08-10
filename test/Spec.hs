module Main (main) where

import Key (KeyNumber(..), KeyType(..))
import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Key" $ do
        it "compares equal values correctly" $ do
            (K_3, A) `shouldBe` (K_3, A)

        it "distinguishes different values" $ do
            (K_3, A) `shouldNotBe` (K_3, B)

    describe "Track" $ do
        it "stores its title and key" $ do
            Track "Thunderfunk" (K_3, A)
                `shouldBe` Track "Thunderfunk" (K_3, A)

-- stack test --test-arguments "--match Key"
-- stack test
