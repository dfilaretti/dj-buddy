module MyLib where

import Camelot (Key)
import Data.List (groupBy, sortOn)
import Data.Map.Strict qualified as Map
import Data.Yaml (FromJSON, decodeFileThrow)
import GHC.Generics (Generic)
import System.Random (randomRIO)
import TestedTransitions
  ( TestedTransition (..),
    TestedTransitionsGraph,
    addEdge,
    emptyGraph,
    getEdges,
  )
import TrackCatalog
  ( Track (..),
    TrackCatalog,
    TrackId,
    addTrack,
    emptyCatalog,
    getAllTrackIds,
    getTrackWithId,
    keyJumpSuccessors,
    sameKeySuccessors,
  )

-----------------------------------------
----- Music Library ---------------------
-----------------------------------------

data MusicLibrary = MusicLibrary
  { trackCatalog :: TrackCatalog,
    transitionGraph :: TestedTransitionsGraph
  }
  deriving (Show)

emptyMusicLibrary :: MusicLibrary
emptyMusicLibrary =
  MusicLibrary
    { trackCatalog = emptyCatalog,
      transitionGraph = emptyGraph
    }

isValidTransition ::
  (TrackId, TestedTransition) ->
  MusicLibrary ->
  Bool
isValidTransition (tid, t) lib =
  and
    [ tid /= t.to,
      tid `elem` validIds,
      t.to `elem` validIds
    ]
  where
    validIds = getAllTrackIds lib.trackCatalog

insertTransition_ ::
  TrackId ->
  TestedTransition ->
  MusicLibrary ->
  MusicLibrary
insertTransition_ tid t lib =
  MusicLibrary {trackCatalog = c', transitionGraph = g'}
  where
    c' = lib.trackCatalog
    g' = addEdge tid t lib.transitionGraph

insertTransition ::
  TrackId ->
  TestedTransition ->
  MusicLibrary ->
  MusicLibrary
insertTransition tid t lib =
  if isValidTransition (tid, t) lib
    then
      insertTransition_ tid t lib
    else
      error $ "insertTransition: track IDs '" ++ tid ++ "' and '" ++ t.to ++ "' must exist in track catalog, and can't be the same!"

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

testedTransitions :: TestedTransitionsGraph -> TrackId -> [Transition]
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

mkPlayListRand ::
  MusicLibrary ->
  Int -> -- number of tracks
  IO [PlaylistMember]
mkPlayListRand lib n =
  do
    tid <- randomElement (Map.keys lib.trackCatalog)
    case tid of
      Just xoxo -> mkPlayList lib xoxo n
      Nothing -> error "ouch"

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

bestTransitions :: [(Transition, Cost)] -> [(Transition, Cost)]
bestTransitions ts =
  case groupBy sameScore (sortOn snd ts) of
    [] -> []
    (xs : _) -> xs
  where
    sameScore a b = snd a == snd b

pickBest :: [(Transition, Cost)] -> IO (Maybe (Transition, Cost))
pickBest ts = randomElement (bestTransitions ts)

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

{---------------------------------------------------------------------------
  Music Library parsing from YAML
----------------------------------------------------------------------------}

readLibFromFile :: FilePath -> IO Lib_raw
readLibFromFile p = decodeFileThrow p :: IO Lib_raw

data Track_raw = Track_raw
  { id :: TrackId,
    title :: String,
    key :: Key
  }
  deriving (Show, Generic)

instance FromJSON Track_raw

data Transition_raw = Transition_raw
  { from :: TrackId,
    to :: TrackId,
    notes :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON Transition_raw

data Lib_raw = Lib_raw
  { tracks :: [Track_raw],
    transitions :: [Transition_raw]
  }
  deriving (Show, Generic)

instance FromJSON Lib_raw

makeTrackCatalogEntry :: Track_raw -> (TrackId, Track)
makeTrackCatalogEntry t_raw =
  (t_raw.id, Track {title = t_raw.title, key = t_raw.key})

makeTrackCatalog :: [Track_raw] -> TrackCatalog
makeTrackCatalog = foldr insertTrack emptyCatalog

insertTrack :: Track_raw -> TrackCatalog -> TrackCatalog
insertTrack tr c = addTrack (makeTrackCatalogEntry tr) c

makeTransitionGraphEntry :: Transition_raw -> (TrackId, TestedTransition)
makeTransitionGraphEntry t =
  (t.from, TestedTransition {to = t.to, notes = t.notes})

makeTransitionGraph :: [Transition_raw] -> MusicLibrary -> MusicLibrary
makeTransitionGraph ts lib = foldr insertRawTransition lib ts

insertRawTransition :: Transition_raw -> MusicLibrary -> MusicLibrary
insertRawTransition tx lib =
  insertTransition from t lib
  where
    (from, t) = makeTransitionGraphEntry tx

makeMusicLibrary :: Lib_raw -> MusicLibrary
makeMusicLibrary l =
  makeTransitionGraph l.transitions intial
  where
    intial =
      MusicLibrary
        { trackCatalog = makeTrackCatalog l.tracks,
          transitionGraph = emptyGraph
        }

{---------------------------------------------------------------------------
  Sample IO actions
----------------------------------------------------------------------------}

mkRandomPlaylist :: Int -> IO ()
mkRandomPlaylist size =
  do
    rawData <- readLibFromFile "src/library.yaml"
    let lib = makeMusicLibrary rawData
    playlist <- mkPlayListRand lib size
    putStr $ printPlaylist lib playlist
