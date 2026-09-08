module MyLib where

import Camelot (Key)
import Data.List (groupBy, sortOn)
import Data.Map.Strict qualified as Map
import Data.Yaml (FromJSON, decodeFileThrow)
import GHC.Generics (Generic)
import MusicLibrary (MusicLibrary (..), insertTransition)
import System.Random (randomRIO)
import TestedTransitions
  ( TestedTransition (..),
    emptyGraph,
  )
import TrackCatalog
  ( Track (..),
    TrackCatalog,
    TrackId,
    addTrack,
    emptyCatalog,
    getTrackWithId,
  )
import Transitions
  ( Cost,
    Transition (..),
    destTrackId,
    scoredTransitions,
  )

-----------------------------------------
----- Transitions -----------------------
-----------------------------------------

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
