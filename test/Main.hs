import Data.List
import MyLib
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [camelotTests, transitionTests]

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
        transitionCost lib transition @?= 2
    ]
