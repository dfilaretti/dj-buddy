import Data.List
import MyLib
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.Map.Strict qualified as Map

instance Arbitrary KeyNumber where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary KeyType where
  arbitrary = elements [minBound .. maxBound]

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [camelotTests, transitionTests, propertyTests, miscTests]

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
            transition = Tested (TestedTransition 42 Nice Nothing)
        transitionCost lib transition @?= 0,
      testCase "same key transitions cost 1" $ do
        let lib = emptyMusicLibrary
            transition = Harmonic SameKey 42
        transitionCost lib transition @?= 1,
      testCase "compatible-key transitions cost 2" $ do
        let lib = emptyMusicLibrary
            transition = Harmonic CompatibleKeys 42
        transitionCost lib transition @?= 2,
      testCase "tested transitions do not appear as harmonic transitions" $ do
        let lib =
                MusicLibrary {
                    transitionGraph = Map.fromList [
                        (0, [TestedTransition {to=1, rating=Omg, notes=Nothing}])
                    ],
                    trackCatalog = Map.fromList[
                        (0, Track "A" (Key K_1 M)),
                        (1, Track "B" (Key K_1 D))],
                    nextTrackId = 0 }
            allTransitions = transitions lib 0
            justHarmonicTransitions = harmonicTransitions lib.trackCatalog 0
            justTestedTransitions = testedTransitions lib.transitionGraph 0
        length justHarmonicTransitions @?= 1
        length justTestedTransitions @?= 1
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
