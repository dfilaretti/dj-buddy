module Main (main) where

import System.Exit (exitFailure)
import Test.QuickCheck (Result, isSuccess, quickCheckResult)

-- QuickCheck generates many arbitrary names and checks this property for each.
prop_helloWorld :: String -> Bool
prop_helloWorld name =
  reverse (reverse greeting) == greeting
  where
    greeting = "Hello, " ++ name ++ "!"

main :: IO ()
main = do
  result <- quickCheckResult prop_helloWorld
  exitOnFailure result

exitOnFailure :: Result -> IO ()
exitOnFailure result
  | isSuccess result = pure ()
  | otherwise = exitFailure
