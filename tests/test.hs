{-# LANGUAGE OverloadedStrings #-}
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
      [ parserTestCase "parser test 1"
        "foo bar" $ [Command "foo bar"]
      ]
    ]
  ]


parserTestCase name input expect =
  testCase name $ parseOkText input @?= expect
