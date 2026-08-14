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
        [ (1, Track "Thunderfunk" (K_3, A))
        , (2, Track "Howl At The Moon" (K_7, B))
        , (3, Track "How Could I Say It" (K_3, A))
        , (4, Track "Feels So Good" (K_4, A))
        , (5, Track "xoxo" (K_2, A))
        , (6, Track "xoxdo" (K_8, A))
        , (7, Track "xoxfeo" (K_3, B))
        ]

graph :: Graph
graph =
    Map.fromList
        [ (1,
            [ Edge 2 SameKey
            , Edge 3 SameKey
            ])
        , (2, [Edge 2 Tested])
        , (7, [Edge 5 Harmonic])
        ]
