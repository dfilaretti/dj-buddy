module Track
  ( TrackId,
    Track (..),
  )
where

import qualified Camelot

type TrackId = Int

data Track = Track
  { title :: String,
    key :: Camelot.Key
    -- , author: String
    -- etc.
  }
  deriving (Eq, Show)
