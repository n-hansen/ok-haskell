#!/usr/bin/env stack
{- stack
  script
  --resolver lts-13.25
  --package turtle
  --package text
  --package megaparsec
-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
module Ok where

import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.Text            as T
import           Data.Void
import           Debug.Trace
import           Text.Megaparsec      as MP
import           Text.Megaparsec.Char as MP
import           Turtle               hiding (Parser, sepBy)

main :: IO ()
main = echo "Hello World!"


--- Types ---

data Root
data Child

data OkDocument a where
  DocumentRoot :: [OkDocument Child] -- ^ Children
               -> OkDocument Root
  DocumentSection :: Text -- ^ Section name
                  -> [OkDocument Child] -- ^ Children
                  -> OkDocument Child
  Command :: Text -- ^ Command string
          -> Maybe Text -- ^ Doc string
          -> Maybe Text -- ^ Alias
          -> OkDocument Child

deriving instance Show (OkDocument Child)
deriving instance Show (OkDocument Root)
deriving instance Eq (OkDocument Child)
deriving instance Eq (OkDocument Root)


-- data OkDocument = Command { commandString :: Text
--                           , docString     :: Maybe Text
--                           , alias         :: Maybe Text
--                           }
--                 | Section { sectionName :: Maybe Text
--                           , children    :: [OkDocument] }
--                 deriving (Eq,Show)

--- File Parsing ---

type Parser = Parsec Void Text

endOfLine :: Parser ()
endOfLine = void MP.newline <|> MP.eof

cmdParser :: Parser (OkDocument Child)
cmdParser = do
  a <- aliasParser
  cs <- commandStringParser
  ds <- docStringParser
  endOfLine
  if cs == mempty
    then failure Nothing mempty
    else return $ Command cs ds a

  where
    commandStringParser :: Parser Text
    commandStringParser = strip <$> takeWhileP (Just "command string") (and <$> sequence [(/= '#'), (/= '\n')])

    docStringParser :: Parser (Maybe Text)
    docStringParser = optional $ do
      takeWhile1P Nothing (== '#')
      ds <- takeWhileP (Just "doc string") (/= '\n')
      return $ strip ds

    aliasParser :: Parser (Maybe Text)
    aliasParser = optional . try $ do
      a <- takeWhile1P (Just "alias") isAlphaNum
      MP.char ':'
      MP.space
      return a

childrenParser :: Int -> Parser [OkDocument Child]
childrenParser minLevel =
  fmap catMaybes
  . MP.many
  . MP.choice
  $ [ Just <$> try cmdParser
    , Just <$> try (sectionParser minLevel)
    -- , try emptyLine
    ]

  where
    emptyLine =
      takeWhileP Nothing (== ' ') >> endOfLine >> return Nothing


sectionParser :: Int -> Parser (OkDocument Child)
sectionParser minLevel = do
  (heading, indent) <- headingParser
  children <- childrenParser (indent + 1)
  return $ DocumentSection heading children

  where
    headingParser :: Parser (Text, Int)
    headingParser = do
      indent <- T.length <$> takeWhile1P (Just "heading prefix") (== '#')
      takeWhileP Nothing (== ' ')
      if indent >= minLevel
        then (, indent) <$> takeWhileP (Just "heading") (/= '\n') <* endOfLine
        else failure Nothing mempty


documentParser :: Parser (OkDocument Root)
documentParser = DocumentRoot <$> childrenParser 1


parseOkText :: Text -> Maybe (OkDocument Root)
parseOkText = parseMaybe documentParser
