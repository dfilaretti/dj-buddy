module Examples
    ( library
    , graph
    ) where

import qualified Data.Map.Strict as Map
import Key (KeyNumber(..), KeyType(..))
import Lib

library :: Library
library =
    Map.fromList
        [ (TrackId 1, Track "Thunderfunk" (K_3, A))
        , (TrackId 2, Track "Howl At The Moon" (K_7, B))
        , (TrackId 3, Track "How Could I Say It" (K_1, A))
        , (TrackId 4, Track "Feels So Good" (K_11, B))
        ]

graph :: Graph
graph =
    Map.fromList
        [ (TrackId 1,
            [ Edge (TrackId 2) SameKey
            , Edge (TrackId 3) SameKey
            ])
        , (TrackId 2, [Edge (TrackId 2) Tested])
        ]
