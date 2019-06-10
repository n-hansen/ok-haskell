import           Test.Tasty
import           Test.Tasty.HUnit

import           Ok               hiding (main)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "tests"
  [ testGroup "unit tests"
    [ testGroup ".ok file parser"
      []
    ]
  ]
