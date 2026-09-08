module Utils where

import System.Random (randomRIO)

randomElement :: [a] -> IO (Maybe a)
randomElement [] = pure Nothing
randomElement xs = do
  i <- randomRIO (0, length xs - 1)
  pure (Just (xs !! i))
