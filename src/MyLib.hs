module MyLib where

import Data.List (groupBy, sortOn)
import Data.Map.Strict qualified as Map
import System.Random (randomRIO)

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
  IO [PlaylistMember]
mkPlayList lib tid n =
  mkPlayList_ lib tid n [StartingTrack tid]

mkPlayList_ ::
  MusicLibrary ->
  TrackId -> -- starting track
  Int -> -- number of tracks
  [PlaylistMember] -> -- accumulated result
  IO [PlaylistMember]
mkPlayList_ lib tid n ts
  | length ts == n = return ts
  | otherwise =
      do
        trans <- pickBest allTrans
        case trans of
          Nothing -> return ts
          Just ((trans_, _)) ->
            mkPlayList_ lib (destTrackId trans_) n (ts ++ [MiddleTrack trans_])
  where
    allTrans = filter notUsed $ scoredTransitions lib tid
    usedTracks = getTraskIds_ ts
    notUsed (t, _) = not $ destTrackId t `elem` usedTracks

pickBest :: [(Transition, Cost)] -> IO (Maybe (Transition, Cost))
pickBest ts =
  case groupBy sameScore ts of
    [] -> return Nothing
    (tsGroup : _) -> do
      pick <- randomElement tsGroup
      case pick of
        Nothing -> return Nothing
        Just pick -> return (Just pick)
  where
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

printPlaylist :: MusicLibrary -> [PlaylistMember] -> String
printPlaylist lib = unlines . map prettyPrintMember
  where
    prettyPrintMember (StartingTrack tid) =
      show tid ++ ": " ++ trackDescription tid
    prettyPrintMember (MiddleTrack transition) =
      prettyPrintTransition lib transition

    trackDescription tid =
      case getTrackWithId tid lib.trackCatalog of
        Just track -> track.title ++ " " ++ show track.key
        Nothing -> "ERROR"

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

{---------------------------------------------------------------------------
  Randomness
----------------------------------------------------------------------------}

randomElement :: [a] -> IO (Maybe a)
randomElement [] = pure Nothing
randomElement xs = do
  i <- randomRIO (0, length xs - 1)
  pure (Just (xs !! i))
