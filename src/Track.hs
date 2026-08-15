module Track
  ( TrackId,
    Track (..),
  )
where

import qualified Key as Key

type TrackId = Int

data Track = Track
  { title :: String,
    key :: Key.Key
    -- , author: String
    -- etc.
  }
  deriving (Eq, Show)
