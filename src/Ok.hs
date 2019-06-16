#!/usr/bin/env stack
{- stack
  script
  --resolver lts-13.25
  --package text
  --package megaparsec
  --package prettyprinter
  --package prettyprinter-ansi-terminal
  --package shelly
  --package optparse-applicative
  --package mtl
  --package containers
-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
module Ok where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Map.Strict                           (Map)
import qualified Data.Map.Strict                           as Map
import           Data.Maybe
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Void
import qualified Options.Applicative                       as Opt
import           Prelude                                   hiding (FilePath)
import           Shelly
import           Text.Megaparsec                           as MP
import           Text.Megaparsec.Char                      as MP
import           Text.Megaparsec.Error                     as MP


--- Top Level Functions ---


main :: IO ()
main = parseOpts >>= runProgram


type ErrorSh = ExceptT String Sh

data RunMode = Display
             | GetCmd Text
             deriving (Show)


runProgram :: RunMode -> IO ()
runProgram mode = shelly . printErrors $ go mode
  where
    go :: RunMode -> ErrorSh ()
    go Display =
      lift . echo =<< renderStrict . layoutPretty defaultLayoutOptions . render <$> readOkFile

    go (GetCmd identifier) =
      lift . echo_n =<< lookupCommand identifier =<< readOkFile

    printErrors :: ErrorSh () -> Sh ()
    printErrors errSh = do
      result <- runExceptT errSh
      case result of
        Right () -> pure ()
        Left err -> do
          echo_err ("Ok Error: " <> T.pack err)
          quietExit 1


--- OkDocument ---


data Root
data Child

data OkDocument a where
  DocumentRoot :: [OkDocument Child] -- ^ Children
               -> Map Text Text -- ^ Lookup table
               -> OkDocument Root

  DocumentSection :: Text -- ^ Section name
                  -> [OkDocument Child] -- ^ Children
                  -> OkDocument Child

  Command :: Text -- ^ Command string
          -> Text -- ^ Alias
          -> Maybe Text -- ^ Doc string
          -> OkDocument Child

deriving instance Show (OkDocument Child)
deriving instance Show (OkDocument Root)
deriving instance Eq (OkDocument Child)
deriving instance Eq (OkDocument Root)


--- Command Line Opts ---


parseOpts :: IO RunMode
parseOpts = Opt.execParser $ Opt.info parser desc
  where
    desc =
      Opt.fullDesc
      <> Opt.header "ok -- makefiles for humans"

    parser =
      (getCmdParser <|> displayParser) <**> Opt.helper

    getCmdParser =
      GetCmd <$> Opt.strArgument ( Opt.metavar "COMMAND"
                                 )

    displayParser = pure Display


--- File Parsing ---


type Parser = Parsec Void Text
type ChildParser = RWST Int (Map Text Text) Int Parser


cmdParser :: ChildParser (OkDocument Child)
cmdParser = do
  a <- lift aliasParser
  cs <- lift commandStringParser
  ds <- lift docStringParser
  lift endOfLine
  if cs == mempty
    then failure Nothing mempty
    else do
    alias <- case a of
               Just alias -> pure alias
               Nothing -> do
                 alias <- gets $ T.pack . show
                 modify (+ 1)
                 pure alias
    tell $ Map.singleton alias cs
    return $ Command cs alias ds

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


childrenParser :: ChildParser [OkDocument Child]
childrenParser =
  fmap catMaybes
  . MP.many
  . MP.choice
  $ [ Just <$> try cmdParser
    , Just <$> try sectionParser
    , try emptyLine
    ]

  where
    emptyLine = do
      takeWhileP Nothing (== ' ')
      MP.newline
      return Nothing


sectionParser :: ChildParser (OkDocument Child)
sectionParser = do
  (heading, indent) <- headingParser
  children <- local (const indent) childrenParser
  return $ DocumentSection heading children

  where
    headingParser :: ChildParser (Text, Int)
    headingParser = do
      currLevel <- ask
      indent <- lift $ T.length <$> takeWhile1P (Just "heading prefix") (== '#')
      takeWhileP Nothing (== ' ')
      lift $ if indent > currLevel
             then (, indent) <$> takeWhileP (Just "heading") (/= '\n') <* endOfLine
             else failure Nothing mempty


documentParser :: Parser (OkDocument Root)
documentParser = uncurry DocumentRoot <$> evalRWST childrenParser 0 1


parseOkText :: String -> Text -> Either String (OkDocument Root)
parseOkText filename = first errorBundlePretty . parse documentParser filename


endOfLine :: Parser ()
endOfLine = void MP.newline <|> MP.eof


--- Document Rendering ---


render :: OkDocument Root -> Doc AnsiStyle
render (DocumentRoot topLevelChildren _) = go emptyDoc 1 (computeOffsets topLevelChildren) topLevelChildren
  where
    dsPad = 2
    aliasPad = 1

    go :: Doc AnsiStyle -> Int -> (Int, Int) -> [OkDocument Child] -> Doc AnsiStyle
    go doc depth offsets@(dsOffset, aliasOffset) elems =
      let
        commandPrefix :: Text -> Doc AnsiStyle -> Doc AnsiStyle
        commandPrefix alias doc =
          width (annotate (color Green) $ pretty alias <> ":") (\w -> indent (aliasOffset - w) doc)

        commandSuffix :: Maybe Text -> Doc AnsiStyle -> Doc AnsiStyle
        commandSuffix ds doc =
          case ds of
            Nothing -> doc <> hardline
            Just dsStr ->
              width doc (\w -> annotate (color Blue) $
                               indent (dsOffset - w + aliasOffset) $ "#" <+> (align . sep . fmap pretty . T.words $ dsStr))
              <> hardline
      in
        case elems of
          [] -> doc

          Command name alias ds : rest ->
            let line = pretty name
                       & commandPrefix alias
                       & commandSuffix ds
            in
              go (doc <> line) depth offsets rest

          DocumentSection title children : rest ->
            let header = annotate (color Red) $
                         pretty (T.replicate depth "#") <+> pretty title <> hardline
                childDoc = go (doc <> header) (depth + 1) (computeOffsets children) children
            in
              go childDoc depth offsets rest


    computeOffsets =
      foldl' (\acc@(dso, ao) e -> case e of
                                    Command cmd a _ ->
                                      ( max dso (T.length cmd + dsPad)
                                      , max ao (T.length a + 1 + aliasPad)
                                      )
                                    _ -> acc
             ) (0,2+aliasPad)


--- File I/O ---


getOkFilePath :: ErrorSh FilePath
getOkFilePath = do
  path <- lift $ (</> (".ok" :: FilePath)) <$> pwd
  exists <- lift $ test_f path
  if exists
    then return path
    else throwError $ "Couldn't find file " <> show path


readOkFile :: ErrorSh (OkDocument Root)
readOkFile = do
  okFile <- getOkFilePath
  liftEither . parseOkText (show okFile) =<< lift (readfile okFile)


--- Command Execution ---


lookupCommand :: Text -> OkDocument Root -> ErrorSh Text
lookupCommand ref (DocumentRoot _ table) =
  case Map.lookup ref table of
    Nothing -> throwError $ "Couldn't find command " <> T.unpack ref
    Just cmd -> pure cmd
