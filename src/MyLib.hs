module MyLib (printLib) where

import Data.List (groupBy, sortOn)
import Data.Map.Strict qualified as Map

{---------------------------------------------------------------------------
  Camelot wheel
----------------------------------------------------------------------------}

data Key = Key KeyNumber KeyType deriving (Eq)

data KeyNumber
  = K_1
  | K_2
  | K_3
  | K_4
  | K_5
  | K_6
  | K_7
  | K_8
  | K_9
  | K_10
  | K_11
  | K_12
  deriving (Eq, Ord, Enum, Bounded)

data KeyType
  = D
  | M
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Show KeyNumber where
  show K_1 = "1"
  show K_2 = "2"
  show K_3 = "3"
  show K_4 = "4"
  show K_5 = "5"
  show K_6 = "6"
  show K_7 = "7"
  show K_8 = "8"
  show K_9 = "9"
  show K_10 = "10"
  show K_11 = "11"
  show K_12 = "12"

instance Show Key where
  show (Key n t) = show n ++ show t

compatibleKeys :: Key -> [Key]
compatibleKeys k =
  [increment k, decrement k, relative k]

incrementKeyNumber :: Int -> KeyNumber -> KeyNumber
incrementKeyNumber n k =
  toEnum ((fromEnum k + n) `mod` 12)

add :: Int -> Key -> Key
add n (Key number keyType) =
  Key (incrementKeyNumber n number) keyType

relative :: Key -> Key
relative (Key number D) = Key number M
relative (Key number M) = Key number D

increment, decrement :: Key -> Key
increment = add 1
decrement = add (-1)

-----------------------------------------
----- Tracks & Catalog  -----------------
-----------------------------------------

type TrackId = Int

data Track
  = Track
  { title :: String,
    key :: Key
  }
  deriving (Eq)

instance Show Track where
  show (Track trackTitle trackKey') =
    "\"" ++ trackTitle ++ "\" (" ++ show trackKey' ++ ")"

type TrackCatalog = Map.Map TrackId Track

emptyCatalog :: TrackCatalog
emptyCatalog = Map.empty

addTrack :: TrackId -> Track -> TrackCatalog -> TrackCatalog
addTrack = Map.insert

getTracks :: (Track -> Bool) -> TrackCatalog -> [TrackId]
getTracks predicate =
  Map.keys . Map.filter predicate

getTracksWithKey :: Key -> TrackCatalog -> [TrackId]
getTracksWithKey desiredKey =
  getTracks ((== desiredKey) . (.key))

sameKeySuccessors :: TrackId -> TrackCatalog -> [TrackId]
sameKeySuccessors trackId catalog =
  maybe [] (filter (/= trackId) . (`getTracksWithKey` catalog)) (trackKey trackId catalog)

keyJumpSuccessors :: TrackId -> TrackCatalog -> [TrackId]
keyJumpSuccessors trackId catalog =
  maybe [] (flip getTracksWithKeys catalog . compatibleKeys) (trackKey trackId catalog)

harmonicSuccessors :: TrackId -> TrackCatalog -> [TrackId]
harmonicSuccessors trackId catalog =
  sameKeySuccessors trackId catalog ++ keyJumpSuccessors trackId catalog

getTracksWithKeys :: [Key] -> TrackCatalog -> [TrackId]
getTracksWithKeys keys catalog =
  concatMap (`getTracksWithKey` catalog) keys

trackKey :: TrackId -> TrackCatalog -> Maybe Key
trackKey trackId catalog =
  (.key) <$> Map.lookup trackId catalog

getTrackWithId :: TrackId -> TrackCatalog -> Maybe Track
getTrackWithId = Map.lookup

-----------------------------------------
----- Confirmed transitions graph -------
-----------------------------------------

type Graph = Map.Map TrackId [TestedTransition]

data UserRating
  = Nice
  | Omg
  deriving (Show, Eq, Ord)

data TestedTransition = TestedTransition
  { to :: TrackId,
    rating :: UserRating,
    notes :: Maybe String
  }
  deriving (Eq, Show)

emptyGraph :: Graph
emptyGraph = Map.empty

addEdge :: TrackId -> TestedTransition -> Graph -> Graph
addEdge trackId edge =
  Map.insert trackId [edge]

getEdges :: TrackId -> Graph -> [TestedTransition]
getEdges = Map.findWithDefault []

getEdge :: TrackId -> TrackId -> Graph -> Maybe TestedTransition
getEdge tFrom tTo g =
  case filter (\e -> e.to == tTo) (getEdges tFrom g) of
    [] -> Nothing
    [x] -> Just x
    _ -> error "multiple edges with same destination - why?"

confirmedSuccessors :: TrackId -> Graph -> [TrackId]
confirmedSuccessors trackId = map (.to) . getEdges trackId

-----------------------------------------
----- Music Library ---------------------
-----------------------------------------

data MusicLibrary = MusicLibrary
  { trackCatalog :: TrackCatalog,
    transitionGraph :: Graph,
    nextTrackId :: Int
  }
  deriving (Show)

emptyMusicLibrary :: MusicLibrary
emptyMusicLibrary =
  MusicLibrary
    { trackCatalog = emptyCatalog,
      transitionGraph = emptyGraph,
      nextTrackId = 0
    }

-----------------------------------------
----- Transitions -----------------------
-----------------------------------------

data HarmonicTransType
  = SameKey
  | CompatibleKeys
  deriving (Show, Eq, Ord)

data Transition
  = Tested TestedTransition
  | Harmonic HarmonicTransType TrackId
  deriving (Show, Eq)

type Cost = Int

destTrackId :: Transition -> TrackId
destTrackId (Tested t) = t.to
destTrackId (Harmonic _ t) = t

destTrackIds :: [Transition] -> [TrackId]
destTrackIds ts = map destTrackId ts

destTrack :: TrackCatalog -> Transition -> Maybe Track
destTrack c t =
  getTrackWithId (destTrackId t) c

scoredNextTracks :: MusicLibrary -> TrackId -> [(Maybe Track, Cost)]
scoredNextTracks musicLibrary trackId =
  map getTrack (scoredTransitions musicLibrary trackId)
  where
    getTrack (t, s) =
      (destTrack musicLibrary.trackCatalog t, s)

scoredTransitions :: MusicLibrary -> TrackId -> [(Transition, Cost)]
scoredTransitions musicLibrary =
  sortOn snd . scoreTransitions musicLibrary . transitions musicLibrary

sameKeyTransitions :: TrackCatalog -> TrackId -> [Transition]
sameKeyTransitions cat tid = map (Harmonic SameKey) (sameKeySuccessors tid cat)

compatibleKeysTransitions :: TrackCatalog -> TrackId -> [Transition]
compatibleKeysTransitions cat tid = map (Harmonic CompatibleKeys) (keyJumpSuccessors tid cat)

harmonicTransitions :: TrackCatalog -> TrackId -> [Transition]
harmonicTransitions cat tid =
  sameKey ++ compatibleKeys
  where
    sameKey = sameKeyTransitions cat tid
    compatibleKeys = compatibleKeysTransitions cat tid

testedTransitions :: Graph -> TrackId -> [Transition]
testedTransitions g tid = map Tested (getEdges tid g)

transitions :: MusicLibrary -> TrackId -> [Transition]
transitions lib tid = tested ++ (filter isNotTested harmonic)
  where
    isNotTested t = not $ elem (destTrackId t) (destTrackIds tested)
    harmonic = harmonicTransitions lib.trackCatalog tid
    tested = testedTransitions lib.transitionGraph tid

scoreTransitions :: MusicLibrary -> [Transition] -> [(Transition, Cost)]
scoreTransitions lib ts =
  zip ts scores
  where
    scores = map (transitionCost lib) ts

transitionCost :: MusicLibrary -> Transition -> Cost
transitionCost _ (Tested _) = 0
transitionCost _ (Harmonic t _) = cost t
  where
    cost SameKey = 1
    cost CompatibleKeys = 2

-----------------------------------------
----- Playlist gen ----------------------
-----------------------------------------

data PlaylistMember
  = StartingTrack TrackId
  | MiddleTrack Transition
  deriving (Show)

getTrackId_ :: PlaylistMember -> TrackId
getTrackId_ (StartingTrack tid) = tid
getTrackId_ (MiddleTrack t) = destTrackId t

getTraskIds_ :: [PlaylistMember] -> [TrackId]
getTraskIds_ = fmap getTrackId_

mkPlayList ::
  MusicLibrary ->
  TrackId -> -- starting track
  Int -> -- number of tracks
  [PlaylistMember]
mkPlayList lib tid n =
  mkPlayList_ lib tid n [StartingTrack tid]

mkPlayList_ ::
  MusicLibrary ->
  TrackId -> -- starting track
  Int -> -- number of tracks
  [PlaylistMember] -> -- accumulated result
  [PlaylistMember]
mkPlayList_ lib tid n ts
  | length ts == n = ts
  | otherwise = mkPlayList_ lib tid' n (ts ++ [MiddleTrack trans])
  where
    trans = fst $ pickBest $ filter notUsed $ scoredTransitions lib tid
    tid' = destTrackId trans
    usedTracks = getTraskIds_ ts
    notUsed (t, _) = not $ destTrackId t `elem` usedTracks

-- TODO: evantually this shoould return a random choice among same-rating ones
pickBest :: [(Transition, Cost)] -> (Transition, Cost)
pickBest ts =
  pickOne $ head $ groupBy sameScore ts
  where
    pickOne = head -- TODO: eventually pic
    sameScore a b = snd a == snd b

-----------------------------------------
----- Pretty Print ----------------------
-----------------------------------------

prettyPrintTransition :: MusicLibrary -> Transition -> String
prettyPrintTransition lib (Tested t) =
  show t.to ++ ": " ++ title ++ " " ++ key ++ " [" ++ show t ++ "]"
  where
    title = case getTrackWithId t.to lib.trackCatalog of
      Just t -> t.title
      Nothing -> "ERROR"
    key = case getTrackWithId t.to lib.trackCatalog of
      Just t -> show t.key
      Nothing -> "ERROR"
prettyPrintTransition lib (Harmonic t tID) =
  show tID ++ ": " ++ title ++ " " ++ key ++ " [" ++ show t ++ "]"
  where
    title = case getTrackWithId tID lib.trackCatalog of
      Just t -> t.title
      Nothing -> "ERROR"
    key = case getTrackWithId tID lib.trackCatalog of
      Just t -> show t.key
      Nothing -> "ERROR"

printTransitions :: MusicLibrary -> [Transition] -> IO ()
printTransitions lib = putStr . unlines . map (prettyPrintTransition lib)

prettyPrintTransition' :: MusicLibrary -> (Transition, Cost) -> String
prettyPrintTransition' lib (t, c) = prettyPrintTransition lib t ++ " [cost = " ++ show c ++ "]"

printTransitions' :: MusicLibrary -> [(Transition, Cost)] -> IO ()
printTransitions' lib = putStr . unlines . map (prettyPrintTransition' lib)

printStuff :: MusicLibrary -> TrackId -> String
printStuff lib tid =
  decoration ++ heading ++ msg ++ decoration
  where
    decorationLen = 100
    decoration = replicate decorationLen '-' ++ "\n"
    title = case getTrackWithId tid lib.trackCatalog of
      Just t -> t.title
      Nothing -> "ERROR"
    key = case getTrackWithId tid lib.trackCatalog of
      Just t -> t.key
      Nothing -> error "no key"
    heading = "Good transitions from \"" ++ title ++ "\" (" ++ show key ++ ")\n\n"
    msg = unlines $ map (prettyPrintTransition' lib) $ scoredTransitions lib tid

printTrackCatalog :: TrackCatalog -> IO ()
printTrackCatalog =
  putStr . unlines . map formatTrack . Map.toList
  where
    formatTrack (trackId, track) =
      show trackId ++ ": " ++ show track

-----------------------------------------
----- Basic CLI -------------------------
-----------------------------------------

printLib :: IO ()
printLib = printTrackCatalog library.trackCatalog

nextTracks :: TrackId -> IO ()
nextTracks tid = putStr $ printStuff library tid

-----------------------------------------
----- Example data ----------------------
-----------------------------------------

-- Built-in data
another_summer_id = 0

how_could_i_say_it_id = 1

nothing_is_impossible_id = 2

spirit_id = 3

mahon_id = 4

howl_at_the_moon_id = 5

melody_express_id = 6

tribe_of_the_sun_id = 7

inferno_id = 8

against_the_wind_id :: TrackId
against_the_wind_id = 9

resilience_id = 10

too_much_rain_id = 11

aeterna_id = 12

set_me_free_id = 13

thunderfunk_id = 14

polkatorju_id = 15

falling_id = 16

feels_so_good_id = 17

colossal_id = 18

aniara_id = 19

marulio_id = 20

sorrento_id = 21

dan_stone_feel_so_good_id = 22

doubt_and_fear_id = 23

okinawa_id = 24

united_in_love_id = 25

ode_to_autumn_id = 26

breath_easy_id = 27

without_u_id = 28

lost_dimension = 29

library :: MusicLibrary
library =
  MusicLibrary
    { nextTrackId = 100,
      trackCatalog =
        Map.fromList
          [ (another_summer_id, Track "Another Summer" (Key K_3 D)),
            (how_could_i_say_it_id, Track "How Could I say It" (Key K_10 D)),
            (nothing_is_impossible_id, Track "Nothing Is Impossible" (Key K_7 D)),
            (spirit_id, Track "Spirit" (Key K_9 D)),
            (mahon_id, Track "Mahon" (Key K_8 D)),
            (howl_at_the_moon_id, Track "Howl At The Moon" (Key K_1 M)),
            (melody_express_id, Track "Melody Express" (Key K_3 D)),
            (tribe_of_the_sun_id, Track "Tribe Of The Sun" (Key K_4 D)),
            (inferno_id, Track "Inferno" (Key K_4 D)),
            (against_the_wind_id, Track "Against The Wind" (Key K_10 D)),
            (resilience_id, Track "Resilience" (Key K_5 D)),
            (too_much_rain_id, Track "Too Much Rain" (Key K_1 D)),
            (aeterna_id, Track "Aeterna" (Key K_2 D)),
            (set_me_free_id, Track "Set Me Free" (Key K_9 M)),
            (thunderfunk_id, Track "Thunderfunk" (Key K_1 M)),
            (polkatorju_id, Track "Polkatorju" (Key K_10 D)),
            (falling_id, Track "Falling" (Key K_6 D)),
            (feels_so_good_id, Track "Feels So Good" (Key K_10 D)),
            (colossal_id, Track "Colossal" (Key K_4 M)),
            (aniara_id, Track "Aniara" (Key K_9 D)),
            (marulio_id, Track "Marulio" (Key K_1 D)),
            (sorrento_id, Track "Sorrento" (Key K_9 M)),
            -- batch 2
            (dan_stone_feel_so_good_id, Track "Feel So Good (Dan Stone)" (Key K_2 M)),
            (doubt_and_fear_id, Track "Doubt And Fear" (Key K_8 D)),
            (okinawa_id, Track "Okinawa" (Key K_11 D)),
            (united_in_love_id, Track "United In Love" (Key K_12 D)),
            (ode_to_autumn_id, Track "Ode to Autumn" (Key K_8 M)),
            (breath_easy_id, Track "Breath Easy" (Key K_11 M)),
            (without_u_id, Track "Without U (Steve RMX)" (Key K_2 D)),
            (lost_dimension, Track "Lost Dimnsion" (Key K_2 M))
          ],
      transitionGraph =
        Map.fromList
          [ ( melody_express_id,
              [ TestedTransition
                  { to = another_summer_id,
                    rating = Omg,
                    notes = Nothing
                  }
              ]
            ),
            ( thunderfunk_id,
              [ TestedTransition
                  { to = howl_at_the_moon_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( howl_at_the_moon_id,
              [ TestedTransition
                  { to = colossal_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( nothing_is_impossible_id,
              [ TestedTransition
                  { to = mahon_id,
                    rating = Nice,
                    notes = Nothing
                  },
                TestedTransition
                  { to = falling_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( aniara_id,
              [ TestedTransition
                  { to = how_could_i_say_it_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( feels_so_good_id,
              [ TestedTransition
                  { to = polkatorju_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( polkatorju_id,
              [ TestedTransition
                  { to = resilience_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( set_me_free_id,
              [ TestedTransition
                  { to = spirit_id,
                    rating = Nice,
                    notes = Nothing
                  },
                TestedTransition
                  { to = sorrento_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( spirit_id,
              [ TestedTransition
                  { to = aniara_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( colossal_id,
              [ TestedTransition
                  { to = inferno_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( resilience_id,
              [ TestedTransition
                  { to = falling_id,
                    rating = Omg,
                    notes = Just "mashup: play falling vox over resilience!"
                  }
              ]
            ),
            ( mahon_id,
              [ TestedTransition
                  { to = spirit_id,
                    rating = Nice,
                    notes = Nothing
                  },
                TestedTransition
                  { to = feels_so_good_id,
                    rating = Nice,
                    notes = Nothing
                  },
                TestedTransition
                  { to = ode_to_autumn_id,
                    rating = Omg,
                    notes = Nothing
                  }
              ]
            ),
            ( against_the_wind_id,
              [ TestedTransition
                  { to = feels_so_good_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( sorrento_id,
              [ TestedTransition
                  { to = set_me_free_id,
                    rating = Nice,
                    notes = Nothing
                  },
                TestedTransition
                  { to = spirit_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( marulio_id,
              [ TestedTransition
                  { to = melody_express_id,
                    rating = Nice,
                    notes = Nothing
                  },
                TestedTransition
                  { to = without_u_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( falling_id,
              [ TestedTransition
                  { to = mahon_id,
                    rating = Nice,
                    notes = Nothing
                  },
                TestedTransition
                  { to = marulio_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( lost_dimension,
              [ TestedTransition
                  { to = without_u_id,
                    rating = Nice,
                    notes = Nothing
                  },
                TestedTransition
                  { to = colossal_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( without_u_id,
              [ TestedTransition
                  { to = aeterna_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            ),
            ( okinawa_id,
              [ TestedTransition
                  { to = against_the_wind_id,
                    rating = Nice,
                    notes = Nothing
                  }
              ]
            )
          ]
    }
