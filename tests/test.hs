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
        $ DocumentRoot [Command "foo bar" Nothing Nothing]
      , parserTestCase "parser test 2"
        "foo bar # apply foo to bar"
        $ DocumentRoot [Command "foo bar" (Just "apply foo to bar") Nothing]
      , parserTestCase "parser test 3"
        ( T.unlines [ "foo bar # apply foo to bar"
                    , "baz"
                    , "quux -v # verbose quux"
                    ]
        )
        $ DocumentRoot [ Command "foo bar" (Just "apply foo to bar") Nothing
                       , Command "baz" Nothing Nothing
                       , Command "quux -v" (Just "verbose quux") Nothing
                       ]
      , parserTestCase "parser test 4"
        ( T.unlines [ "# section"
                    , "foo bar"
                    ]
        )
        $ DocumentRoot [DocumentSection "section" [Command "foo bar" Nothing Nothing]]
      , parserTestCase "parser test 5"
        ( T.unlines [ "#section"
                    , "foo bar # apply foo to bar"
                    , "flub: baz"
                    , "##subsection"
                    , "bort: quux -v # verbose quux"
                    ]
        )
        $ DocumentRoot
        [ DocumentSection "section" [ Command "foo bar" (Just "apply foo to bar") Nothing
                                    , Command "baz" Nothing (Just "flub")
                                    , DocumentSection "subsection" [Command "quux -v" (Just "verbose quux") (Just "bort")]
                                    ]
        ]
      , parserTestCase "parser test 6"
        ( T.unlines [ "foo"
                    , "#h1"
                    , "bar"
                    , "##h2"
                    , "baz"
                    , "##h3"
                    , "quux"
                    , "#h4"
                    , "flub"
                    ]
        )
        $ DocumentRoot
        [ Command "foo" Nothing Nothing
        , DocumentSection "h1" [ Command "bar" Nothing Nothing
                               , DocumentSection "h2"
                                 [Command "baz" Nothing Nothing]
                               , DocumentSection "h3"
                                 [Command "quux" Nothing Nothing]
                               ]
        , DocumentSection "h4" [Command "flub" Nothing Nothing]
        ]
      , parserTestCase "parser test 7"
        "foo\n\nbar\n\n  \nbaz"
        $ DocumentRoot
        [ Command "foo" Nothing Nothing
        , Command "bar" Nothing Nothing
        , Command "baz" Nothing Nothing
        ]
      ]
    ]
  ]


parserTestCase name input expect =
  testCase name $ parseOkText input @?= Just expect
