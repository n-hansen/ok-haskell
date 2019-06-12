{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text                             as T
import           Ok                                    hiding (main)
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

    , testGroup "document render tests"
      [ renderTestCase "render test 1"
        (DocumentRoot [Command "foo" Nothing Nothing])
        "1: foo\n"
      , renderTestCase "render test 2"
        ( DocumentRoot [ Command "foo" Nothing Nothing
                       , Command "bar" Nothing Nothing
                       ]
        )
        ( unlines [ "1: foo"
                  , "2: bar"
                  ]
        )
      , renderTestCase "render test 3"
        (DocumentRoot [Command "foo" (Just "bar baz") Nothing])
        "1: foo  # bar baz\n"
      , renderTestCase "render test 4"
        ( DocumentRoot [ Command "foo" (Just "bar") Nothing
                       , Command "quux" (Just "baz") Nothing
                       ]
        )
        ( unlines [ "1: foo   # bar"
                  , "2: quux  # baz"
                  ]
        )
      , renderTestCase "render test 5"
        ( DocumentRoot [ Command "foo" Nothing Nothing
                       , DocumentSection "h1"
                         [ Command "bar" Nothing Nothing
                         , Command "baz" Nothing Nothing
                         , DocumentSection "h2" [Command "quux" Nothing Nothing]
                         ]
                       ]
        )
        ( unlines [ "1: foo"
                  , "# h1"
                  , "2: bar"
                  , "3: baz"
                  , "## h2"
                  , "4: quux"
                  ]
        )
      , renderTestCase "render test 6"
        ( DocumentRoot [ Command "foo" (Just "foo doc") Nothing
                       , DocumentSection "h1"
                         [ Command "bar baz" (Just "bar baz doc") Nothing
                         , Command "quux" (Just "quux doc") Nothing
                         , DocumentSection "h2" [Command "flub" (Just "flub doc") Nothing]
                         ]
                       ]
        )
        ( unlines [ "1: foo  # foo doc"
                  , "# h1"
                  , "2: bar baz  # bar baz doc"
                  , "3: quux     # quux doc"
                  , "## h2"
                  , "4: flub  # flub doc"
                  ]
        )
      , renderTestCase "render test 7"
        (DocumentRoot [Command "foo" Nothing (Just "bar")])
        "bar: foo\n"
      , renderTestCase "render test 8"
        ( DocumentRoot [ Command "foo" (Just "foo doc") (Just "fooAlias")
                       , DocumentSection "h1"
                         [ Command "bar baz" (Just "bar baz doc") (Just "bb")
                         , Command "quux" (Just "quux doc") Nothing
                         , Command "bort" Nothing (Just "bort")
                         , DocumentSection "h2" [Command "flub" (Just "flub doc") Nothing]
                         ]
                       ]
        )
        ( unlines [ "fooAlias: foo  # foo doc"
                  , "# h1"
                  , "bb:   bar baz  # bar baz doc"
                  , "1:    quux     # quux doc"
                  , "bort: bort"
                  , "## h2"
                  , "2: flub  # flub doc"
                  ]
        )

      ]
    ]
  ]


parserTestCase name input expect =
  testCase name $ parseOkText input @?= Just expect

renderTestCase name document expect =
  testCase name $ (show $ render document) @?= expect
