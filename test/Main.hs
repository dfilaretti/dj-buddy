import Control.Exception (SomeException, evaluate, try)
import Data.List
import Data.Map.Strict qualified as Map
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Domain.Camelot
import Domain.TrackCatalog
import Domain.TestedTransitions
import Domain.MusicLibrary
import Domain.Transitions
import Domain.Utils

instance Arbitrary KeyNumber where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary KeyType where
  arbitrary = elements [minBound .. maxBound]

main = defaultMain tests

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
            transition = Tested (TestedTransition "42" Nothing)
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
        let lib =
              MusicLibrary
                { transitionGraph =
                    Map.fromList
                      [ ("0", [TestedTransition {to = "1", notes = Nothing}])
                      ],
                  trackCatalog =
                    Map.fromList
                      [ ("0", Track "A" (Key K_1 M)),
                        ("1", Track "B" (Key K_1 D))
                      ]
                }
            allTransitions = transitions lib "0"
            harmonic = harmonicTransitions lib.trackCatalog "0"
            tested = testedTransitions lib.transitionGraph "0"
        length harmonic @?= 1
        length tested @?= 1
        length allTransitions @?= 1 -- the harmonic transition was filtered out!
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
          let g = emptyGraph
              t = TestedTransition {to = "1", notes = Nothing}
          addEdge "0" t g
            @?= Map.fromList [("0", [t])],
      testCase "inserting the nth transition" $
        do
          let g = Map.fromList [("0", [t1])]
              t1 = TestedTransition "1" Nothing
              t2 = TestedTransition "2" Nothing
          addEdge "0" t2 g @?= Map.fromList [("0", [t2, t1])],
      testCase "inserting the 'same' transition twice should throw" $
        do
          let g = emptyGraph
              t1 = TestedTransition "1" Nothing
              t2 = TestedTransition "1" (Just "same song, different transition")
          result <- try (evaluate (addEdge "0" t1 $ addEdge "0" t2 g)) :: IO (Either SomeException TestedTransitionsGraph)
          case result of
            Left _ -> pure ()
            Right _ -> assertFailure "expected addEdge to throw"
    ]

musicLibraryTests :: TestTree
musicLibraryTests =
  testGroup
    "MusicLibrary"
    [testCase "to insert a transition in the music library, the to/fromIDs must exist, and must not be the same" $
        do
          let lib =
                Map.fromList
                  [ ("0", Track "A" (read "5D")),
                    ("1", Track "B" (read "6D"))
                  ]
              starting = MusicLibrary lib emptyGraph
          isValidTransition ("0", TestedTransition "1" Nothing) starting @?= True
          isValidTransition ("0", TestedTransition "0" Nothing) starting @?= False
          isValidTransition ("99", TestedTransition "0" Nothing) starting @?= False
          isValidTransition ("0", TestedTransition "99" Nothing) starting @?= False
    ]

trackCatalogTests :: TestTree
trackCatalogTests =
  testGroup
    "Track Catalog"
    [ testCase "basic insert works as expected" $
        do
          let c = emptyCatalog
              t = Track "A" (read "5D")
          addTrack ("0", t) c
            @?= Map.fromList [("0", t)],
      testCase "inserting same track twice should throw" $
        do
          let c = Map.fromList [("0", t)]
              t = Track "A" (read "5D")
          result <- try (evaluate (addTrack ("0", t) c)) :: IO (Either SomeException TrackCatalog)
          case result of
            Left _ -> pure ()
            Right _ -> assertFailure "expected addTrack to throw"
    ]
