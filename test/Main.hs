import Control.Monad (foldM)
import Domain.Camelot
import Domain.MusicLibrary
import Domain.Transitions
import Domain.Utils
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

instance Arbitrary KeyNumber where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary KeyType where
  arbitrary = elements [minBound .. maxBound]

main :: IO ()
main = defaultMain tests

expectRight :: (Show e) => Either e a -> IO a
expectRight = either (fail . ("Expected Right, got Left: " ++) . show) pure

libraryWithTracks :: [(TrackId, Track)] -> IO MusicLibrary
libraryWithTracks tracks =
  expectRight $ foldM (flip withTrack) emptyMusicLibrary tracks

-- Error constructors are not exported, so check their public Show representation.
assertLibraryError :: String -> Either MusicLibraryError a -> Assertion
assertLibraryError expected result =
  case result of
    Left err -> show err @?= expected
    Right _ -> assertFailure $ "Expected Left " ++ expected ++ ", got Right"

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ camelotTests,
      graphTests,
      trackCatalogTests,
      transitionTests,
      propertyTests,
      musicLibraryTests,
      miscTests
    ]

camelotTests :: TestTree
camelotTests =
  testGroup
    "Camelot wheel"
    [ testCase "increment 1 -> 2" $ do
        increment (Key K_1 D) @?= Key K_2 D,
      testCase "increment 12 -> 1" $ do
        increment (Key K_12 M) @?= Key K_1 M,
      testCase "relative D -> M" $
        relative (Key K_1 D) @?= Key K_1 M,
      testCase "relative M -> D" $
        relative (Key K_1 M) @?= Key K_1 D,
      testCase "read key number" $
        (read "12" :: KeyNumber) @?= K_12,
      testCase "read key type" $
        (read "M" :: KeyType) @?= M,
      testCase "read key" $
        (read "12M" :: Key) @?= Key K_12 M,
      testCase "compatible keys" $
        compatibleKeys (Key K_1 D)
          @?= [ Key K_2 D,
                Key K_12 D,
                Key K_1 M
              ]
    ]

transitionTests :: TestTree
transitionTests =
  testGroup
    "Transitions"
    [ testCase "tested transitions have zero cost" $ do
        let lib = emptyMusicLibrary
            transition = Tested (TestedTransition Nothing) "42"
        transitionCost lib transition @?= 0,
      testCase "same key transitions cost 1" $ do
        let lib = emptyMusicLibrary
            transition = Harmonic SameKey "42"
        transitionCost lib transition @?= 1,
      testCase "compatible-key transitions cost 2" $ do
        let lib = emptyMusicLibrary
            transition = Harmonic CompatibleKeys "42"
        transitionCost lib transition @?= 2,
      testCase "tested transitions do not appear as harmonic transitions" $ do
        starting <-
          libraryWithTracks
            [ ("0", Track "A" (Key K_1 M)),
              ("1", Track "B" (Key K_1 D))
            ]
        let info = TestedTransition Nothing
        lib <- expectRight $ withTransition "0" "1" info starting
        harmonicTransitions lib "0" @?= [Harmonic CompatibleKeys "1"]
        testedTransitions lib "0" @?= [Tested info "1"]
        transitions lib "0" @?= [Tested info "1"]
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Properties"
    [ QC.testProperty "increment 12 gives same key" $
        \k -> incrementKeyNumber 12 k == k,
      QC.testProperty "relative is involution" $
        \n t -> relative (relative (Key n t)) == Key n t
    ]

miscTests :: TestTree
miscTests =
  testGroup
    "Misc"
    [ testCase "randomElement picks an element from the list" $ do
        result <- randomElement [1, 2, 3]
        assertBool
          "result must be an element of the input"
          (result `elem` map Just [1, 2, 3])
    ]

graphTests :: TestTree
graphTests =
  testGroup
    "Graph"
    [ testCase "inserting the first transition" $
        do
          starting <-
            libraryWithTracks
              [("0", Track "A" (read "5D")), ("1", Track "B" (read "6D"))]
          let t = TestedTransition Nothing
          lib <- expectRight $ withTransition "0" "1" t starting
          getEdges "0" lib @?= [("1", t)]
          getEdges "1" lib @?= [],
      testCase "inserting the nth transition" $
        do
          starting <-
            libraryWithTracks
              [ ("0", Track "A" (read "5D")),
                ("1", Track "B" (read "6D")),
                ("2", Track "C" (read "7D"))
              ]
          let t1 = TestedTransition Nothing
              t2 = TestedTransition (Just "second transition")
          first <- expectRight $ withTransition "0" "1" t1 starting
          lib <- expectRight $ withTransition "0" "2" t2 first
          -- Destinations are now stored in a Map, ordered by track ID.
          getEdges "0" lib @?= [("1", t1), ("2", t2)],
      testCase "inserting the 'same' transition twice should return Left" $
        do
          starting <-
            libraryWithTracks
              [("0", Track "A" (read "5D")), ("1", Track "B" (read "6D"))]
          let t1 = TestedTransition Nothing
              t2 = TestedTransition (Just "same song, different transition")
          lib <- expectRight $ withTransition "0" "1" t1 starting
          assertLibraryError "TestedTransitionAlreadyExists \"0\" \"1\"" $
            withTransition "0" "1" t1 lib
          assertLibraryError "TestedTransitionAlreadyExists \"0\" \"1\"" $
            withTransition "0" "1" t2 lib
          getEdges "0" lib @?= [("1", t1)]
          reverseLib <- expectRight $ withTransition "1" "0" t2 lib
          getEdges "1" reverseLib @?= [("0", t2)]
          getEdges "0" reverseLib @?= [("1", t1)]
    ]

musicLibraryTests :: TestTree
musicLibraryTests =
  testGroup
    "MusicLibrary"
    [ testCase "to insert a transition in the music library, the to/fromIDs must exist, and must not be the same" $
        do
          starting <-
            libraryWithTracks
              [ ("0", Track "A" (read "5D")),
                ("1", Track "B" (read "6D"))
              ]
          let info = TestedTransition Nothing
          lib <- expectRight $ withTransition "0" "1" info starting
          getEdges "0" lib @?= [("1", info)]
          assertLibraryError "TransitionHasSameSourceAndDest \"0\"" $
            withTransition "0" "0" info starting
          assertLibraryError "TransitionHasInvalidTrackId \"99\"" $
            withTransition "99" "0" info starting
          assertLibraryError "TransitionHasInvalidTrackId \"99\"" $
            withTransition "0" "99" info starting
    ]

trackCatalogTests :: TestTree
trackCatalogTests =
  testGroup
    "Track Catalog"
    [ testCase "basic insert works as expected" $
        do
          let t = Track "A" (read "5D")
          lib <- expectRight $ withTrack ("0", t) emptyMusicLibrary
          getPairs lib @?= [("0", t)],
      testCase "inserting same track twice should return Left" $
        do
          let t = Track "A" (read "5D")
          lib <- expectRight $ withTrack ("0", t) emptyMusicLibrary
          assertLibraryError "TrackIdAlreadyExists \"0\"" $ withTrack ("0", t) lib
    ]
