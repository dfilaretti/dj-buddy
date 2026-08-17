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
  deriving (Eq, Show)
