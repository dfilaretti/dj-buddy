module Track
  ( TrackId,
    Track (..),
  )
where

import Camelot (Key)

type TrackId = Int

data Track = Track
  { title :: String,
    key :: Key
    -- , author: String
    -- etc.
  }
  deriving (Eq)

instance Show Track where
  show (Track t k) = "\"" ++ t ++ "\" (" ++ show k ++ ")"
