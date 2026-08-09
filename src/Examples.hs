module Examples
    ( library
    , graph
    ) where

import qualified Data.Map.Strict as Map
import Lib

library :: Library
library =
    Map.fromList
        [ (TrackId 1, Track "Thunderfunk" (A 3))
        , (TrackId 2, Track "Howl At The Moon" (B 7))
        , (TrackId 3, Track "How Could I Say It" (B 1))
        , (TrackId 4, Track "Feels So Good" (B 11))
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
