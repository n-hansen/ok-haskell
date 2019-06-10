#!/usr/bin/env stack
{- stack
  script
  --resolver lts-13.25
  --package turtle
-}
{-# LANGUAGE OverloadedStrings #-}
module Ok where

import Turtle

main :: IO ()
main = echo "Hello World!"
