{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import           Ok               hiding (main)
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "tests"
  [ testGroup "unit tests"
    [ testGroup ".ok file parser"
      [ parserTestCase "parser test 1"
        "foo bar"
        [Command "foo bar" Nothing Nothing]
      , parserTestCase "parser test 2"
        "foo bar # apply foo to bar"
        [Command "foo bar" (Just "apply foo to bar") Nothing]
      , parserTestCase "parser test 3"
        ( T.unlines [ "foo bar # apply foo to bar"
                    , "baz"
                    , "quux -v # verbose quux"
                    ]
        )
        [ Command "foo bar" (Just "apply foo to bar") Nothing
        , Command "baz" Nothing Nothing
        , Command "quux -v" (Just "verbose quux") Nothing
        ]
      , parserTestCase "parser test 4"
        ( T.unlines [ "# comment"
                    , "foo bar"
                    ]
        )
        [Command "foo bar" Nothing Nothing]
      , parserTestCase "parser test 5"
        ( T.unlines [ "foo bar # apply foo to bar"
                    , "flub: baz"
                    , "bort: quux -v # verbose quux"
                    ]
        )
        [ Command "foo bar" (Just "apply foo to bar") Nothing
        , Command "baz" Nothing (Just "flub")
        , Command "quux -v" (Just "verbose quux") (Just "bort")
        ]
      ]
    ]
  ]


parserTestCase name input expect =
  testCase name $ parseOkText input @?= expect
