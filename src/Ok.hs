#!/usr/bin/env stack
{- stack
  script
  --resolver lts-13.25
  --package turtle
  --package text
  --package megaparsec
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Ok where

import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.Text
import           Data.Void
import           Text.Megaparsec
import qualified Text.Megaparsec      as MP
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as MP
import           Turtle               hiding (Parser, sepBy)

main :: IO ()
main = echo "Hello World!"


--- Types ---

data Command = Command { commandString :: Text
                       , docString     :: Maybe Text
                       , alias         :: Maybe Text
                       } deriving (Eq,Show)

--- File Parsing ---

type Parser = Parsec Void Text

cmdParser :: Parser (Maybe Command)
cmdParser = do
  a <- aliasParser
  cs <- csParser
  ds <- dsParser
  if cs == mempty
    then return Nothing
    else return . Just $ Command cs ds a

  where
    csParser :: Parser Text
    csParser = strip <$> takeWhileP (Just "command string") (and <$> sequence [(/= '#'), (/= '\n')])

    dsParser :: Parser (Maybe Text)
    dsParser = optional $ fmap strip $ takeWhile1P Nothing (== '#') *> takeWhileP (Just "doc string") (/= '\n')

    aliasParser :: Parser (Maybe Text)
    aliasParser = optional $ try $ takeWhile1P (Just "alias") isAlphaNum <* MP.char ':' <* space1

parseOkText :: Text -> [Command]
parseOkText = join . maybeToList . parseMaybe multilineParser
  where
    multilineParser :: Parser [Command]
    multilineParser = catMaybes <$> cmdParser `sepBy` MP.newline
