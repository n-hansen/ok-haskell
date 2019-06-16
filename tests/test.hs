{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map.Strict  as Map
import qualified Data.Text        as T
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
        $ DocumentRoot [Command "foo bar" "1" Nothing] [("1", "foo bar")]
      , parserTestCase "parser test 2"
        "foo bar # apply foo to bar"
        $ DocumentRoot [Command "foo bar" "1" (Just "apply foo to bar")] [("1", "foo bar")]
      , parserTestCase "parser test 3"
        ( T.unlines [ "foo bar # apply foo to bar"
                    , "baz"
                    , "quux -v # verbose quux"
                    ]
        )
        $ DocumentRoot
        [ Command "foo bar" "1" (Just "apply foo to bar")
        , Command "baz" "2" Nothing
        , Command "quux -v" "3" (Just "verbose quux")
        ]
        [ ("1", "foo bar")
        , ("2", "baz")
        , ("3", "quux -v")
        ]
      , parserTestCase "parser test 4"
        ( T.unlines [ "# section"
                    , "foo bar"
                    ]
        )
        $ DocumentRoot [DocumentSection "section" [Command "foo bar" "1" Nothing]] [("1", "foo bar")]
      , parserTestCase "parser test 5"
        ( T.unlines [ "#section"
                    , "foo bar # apply foo to bar"
                    , "flub: baz"
                    , "##subsection"
                    , "bort: quux -v # verbose quux"
                    ]
        )
        $ DocumentRoot
        [ DocumentSection "section" [ Command "foo bar" "1" (Just "apply foo to bar")
                                    , Command "baz" "flub" Nothing
                                    , DocumentSection "subsection" [Command "quux -v" "bort" (Just "verbose quux")]
                                    ]
        ]
        [ ("1", "foo bar")
        , ("flub", "baz")
        , ("bort", "quux -v")
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
        [ Command "foo" "1" Nothing
        , DocumentSection "h1" [ Command "bar" "2" Nothing
                               , DocumentSection "h2"
                                 [Command "baz" "3" Nothing]
                               , DocumentSection "h3"
                                 [Command "quux" "4" Nothing]
                               ]
        , DocumentSection "h4" [Command "flub" "5" Nothing]
        ]
        [ ("1", "foo")
        , ("2", "bar")
        , ("3", "baz")
        , ("4", "quux")
        , ("5", "flub")
        ]
      , parserTestCase "parser test 7"
        "foo\n\nbar\n\n  \nbaz"
        $ DocumentRoot
        [ Command "foo" "1" Nothing
        , Command "bar" "2" Nothing
        , Command "baz" "3" Nothing
        ]
        [ ("1", "foo")
        , ("2", "bar")
        , ("3", "baz")
        ]
      ]

    , testGroup "document render tests"
      [ renderTestCase "render test 1"
        (DocumentRoot [Command "foo" "1" Nothing] mempty)
        "1: foo\n"
      , renderTestCase "render test 2"
        ( DocumentRoot [ Command "foo" "1" Nothing
                       , Command "bar" "2" Nothing
                       ] mempty
        )
        ( unlines [ "1: foo"
                  , "2: bar"
                  ]
        )
      , renderTestCase "render test 3"
        (DocumentRoot [Command "foo" "1" (Just "bar baz")] mempty)
        "1: foo  # bar baz\n"
      , renderTestCase "render test 4"
        ( DocumentRoot [ Command "foo" "1" (Just "bar")
                       , Command "quux""2" (Just "baz")
                       ] mempty
        )
        ( unlines [ "1: foo   # bar"
                  , "2: quux  # baz"
                  ]
        )
      , renderTestCase "render test 5"
        ( DocumentRoot [ Command "foo" "1" Nothing
                       , DocumentSection "h1"
                         [ Command "bar" "2" Nothing
                         , Command "baz" "3" Nothing
                         , DocumentSection "h2" [Command "quux" "4" Nothing]
                         ]
                       ] mempty
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
        ( DocumentRoot [ Command "foo" "1" (Just "foo doc")
                       , DocumentSection "h1"
                         [ Command "bar baz" "2" (Just "bar baz doc")
                         , Command "quux" "3" (Just "quux doc")
                         , DocumentSection "h2" [Command "flub" "4" (Just "flub doc")]
                         ]
                       ] mempty
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
        (DocumentRoot [Command "foo" "bar" Nothing] mempty)
        "bar: foo\n"
      , renderTestCase "render test 8"
        ( DocumentRoot [ Command "foo" "fooAlias" (Just "foo doc")
                       , DocumentSection "h1"
                         [ Command "bar baz" "bb" (Just "bar baz doc")
                         , Command "quux" "1" (Just "quux doc")
                         , Command "bort" "bort" Nothing
                         , DocumentSection "h2" [Command "flub" "2" (Just "flub doc")]
                         ]
                       ] mempty
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
      , renderTestCase "render test 9"
        ( DocumentRoot [ Command "foo" "1" Nothing
                       , DocumentSection "h1" [ Command "bar" "2" Nothing
                                              , DocumentSection "h2"
                                                [Command "baz" "3" Nothing]
                                              , DocumentSection "h3"
                                                [Command "quux" "4" Nothing]
                                              ]
                       , DocumentSection "h4" [Command "flub" "5" Nothing]
                       ] mempty
        )
        ( unlines [ "1: foo"
                  , "# h1"
                  , "2: bar"
                  , "## h2"
                  , "3: baz"
                  , "## h3"
                  , "4: quux"
                  , "# h4"
                  , "5: flub"
                  ]
        )
      ]
    ]
  ]


parserTestCase name input expect =
  testCase name $ parseOkText "" input @?= Right expect

renderTestCase name document expect =
  testCase name $ (show $ render document) @?= expect
