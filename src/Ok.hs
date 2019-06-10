#!/usr/bin/env stack
{- stack
  script
  --resolver lts-13.25
  --package turtle
-}
{-# LANGUAGE OverloadedStrings #-}
import Turtle

main = echo "Hello World!"
