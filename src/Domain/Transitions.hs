module Domain.Transitions where

import Data.List (groupBy, sortOn)
import Domain.MusicLibrary
  ( MusicLibrary (..),
    TestedTransition (..),
    Track,
    TrackId,
    getEdges,
    getTrackWithId,
    keyJumpSuccessors,
    sameKeySuccessors,
  )
import Domain.Utils (randomElement)

data HarmonicTransType
  = SameKey
  | CompatibleKeys
  deriving (Show, Eq, Ord)

data Transition
  = Tested TestedTransition TrackId
  | Harmonic HarmonicTransType TrackId
  deriving (Show, Eq)

type Cost = Int

-- TODO: should be able to use takeWhile here
bestTransitions ::
  [(Transition, Cost)] ->
  [(Transition, Cost)]
bestTransitions ts =
  case groupBy sameScore (sortOn snd ts) of
    [] -> []
    (xs : _) -> xs
  where
    sameScore a b = snd a == snd b

destTrackId :: Transition -> TrackId
destTrackId (Tested _ t) = t
destTrackId (Harmonic _ t) = t

destTrackIds :: [Transition] -> [TrackId]
destTrackIds ts = map destTrackId ts

destTrack :: MusicLibrary -> Transition -> Maybe Track
destTrack c t =
  getTrackWithId (destTrackId t) c

scoredNextTracks :: MusicLibrary -> TrackId -> [(Maybe Track, Cost)]
scoredNextTracks musicLibrary trackId =
  map getTrack (scoredTransitions musicLibrary trackId)
  where
    getTrack (t, s) =
      (destTrack musicLibrary t, s)

scoredTransitions :: MusicLibrary -> TrackId -> [(Transition, Cost)]
scoredTransitions musicLibrary =
  sortOn snd . scoreTransitions musicLibrary . transitions musicLibrary

sameKeyTransitions :: MusicLibrary -> TrackId -> [Transition]
sameKeyTransitions cat tid = map (Harmonic SameKey) (sameKeySuccessors tid cat)

compatibleKeysTransitions :: MusicLibrary -> TrackId -> [Transition]
compatibleKeysTransitions cat tid = map (Harmonic CompatibleKeys) (keyJumpSuccessors tid cat)

harmonicTransitions :: MusicLibrary -> TrackId -> [Transition]
harmonicTransitions cat tid =
  sameKey ++ compatibleKeys
  where
    sameKey = sameKeyTransitions cat tid
    compatibleKeys = compatibleKeysTransitions cat tid

testedTransitions :: MusicLibrary -> TrackId -> [Transition]
testedTransitions g tid =
  map
    (\(t, info) -> Tested info t)
    (getEdges tid g)

transitions :: MusicLibrary -> TrackId -> [Transition]
transitions lib tid = tested ++ (filter isNotTested harmonic)
  where
    isNotTested t = not $ elem (destTrackId t) (destTrackIds tested)
    harmonic = harmonicTransitions lib tid
    tested = testedTransitions lib tid

scoreTransitions :: MusicLibrary -> [Transition] -> [(Transition, Cost)]
scoreTransitions lib ts =
  zip ts scores
  where
    scores = map (transitionCost lib) ts

transitionCost :: MusicLibrary -> Transition -> Cost
transitionCost _ (Tested _ _) = 0
transitionCost _ (Harmonic t _) = cost t
  where
    cost SameKey = 1
    cost CompatibleKeys = 2

pickBest ::
  [(Transition, Cost)] ->
  IO (Maybe (Transition, Cost))
pickBest = randomElement . bestTransitions
