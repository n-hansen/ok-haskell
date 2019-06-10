#!/usr/bin/env stack
{- stack
  script
  --resolver lts-13.25
  --package turtle
-}
{-# LANGUAGE OverloadedStrings #-}
module Ok where

import           Data.Text
import           Turtle

main :: IO ()
main = echo "Hello World!"


--- Types ---

data Command = Command { commandString :: Text
                       } deriving (Eq,Show)

--- File Parsing ---

parseOkText :: Text -> [Command]
parseOkText txt = [Command txt]
