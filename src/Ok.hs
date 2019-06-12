#!/usr/bin/env stack
{- stack
  script
  --resolver lts-13.25
  --package turtle
  --package text
  --package megaparsec
  --package prettyprinter
-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
module Ok where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Void
import           Text.Megaparsec           as MP
import           Text.Megaparsec.Char      as MP
import           Turtle                    hiding (Parser)

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
    commandStringParser = T.strip <$> takeWhileP (Just "command string") (and <$> sequence [(/= '#'), (/= '\n')])

    docStringParser :: Parser (Maybe Text)
    docStringParser = optional $ do
      takeWhile1P Nothing (== '#')
      ds <- takeWhileP (Just "doc string") (/= '\n')
      return $ T.strip ds

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
    , try emptyLine
    ]

  where
    emptyLine = do
      takeWhileP Nothing (== ' ')
      MP.newline
      return Nothing


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

--- Document Rendering ---

render :: OkDocument Root -> Doc Void
render (DocumentRoot topLevelChildren) = fst $ go emptyDoc 1 1 (computeOffsets topLevelChildren) topLevelChildren
  where
    dsPad = 2
    aliasPad = 1

    go :: Doc Void -> Int -> Int -> (Int, Int) -> [OkDocument Child] -> (Doc Void, Int)
    go doc count depth offsets@(dsOffset, aliasOffset) elems =
      let
        commandPrefix :: Pretty p => p -> Doc a -> Doc a
        commandPrefix alias doc =
          width (pretty alias <> ":") (\w -> indent (aliasOffset - w) doc)

        commandSuffix :: Maybe Text -> Doc a -> Doc a
        commandSuffix ds doc =
          case ds of
            Nothing -> doc <> hardline
            Just dsStr ->
              width doc (\w -> indent (dsOffset - w + aliasOffset) $ "#" <+> (align . sep . fmap pretty . T.words $ dsStr))
              <> hardline
      in
        case elems of
          [] -> (doc, count)

          Command name ds Nothing : rest ->
            let line = pretty name
                       & commandPrefix count
                       & commandSuffix ds
            in
              go (doc <> line) (count + 1) depth offsets rest

          Command name ds (Just alias) : rest ->
            let line = pretty name
                       & commandPrefix alias
                       & commandSuffix ds
            in
              go (doc <> line) count depth offsets rest

          DocumentSection title children : rest ->
            let header = pretty (T.replicate depth "#") <+> pretty title <> hardline
                (childDoc, count') = go (doc <> header) count (depth + 1) (computeOffsets children) children
            in
              go childDoc count' depth offsets rest


    computeOffsets =
      foldl' (\acc@(dso, ao) e -> case e of
                                    Command cmd _ Nothing ->
                                      ( max dso (T.length cmd + dsPad)
                                      , ao
                                      )
                                    Command cmd _ (Just a) ->
                                      ( max dso (T.length cmd + dsPad)
                                      , max ao (T.length a + 1 + aliasPad)
                                      )
                                    _ -> acc
             ) (0,2+aliasPad)
