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
  --package system-filepath
-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
module Ok where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.RWS.Strict
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
import qualified Filesystem.Path                           as FP
import qualified Options.Applicative                       as Opt
import           Prelude                                   hiding (FilePath)
import           Shelly.Lifted
import           Text.Megaparsec                           as MP
import           Text.Megaparsec.Char                      as MP

--- Top Level Functions ---

main :: IO ()
main = parseOpts >>= runProgram


type OkApp = ExceptT String (ReaderT RunOpts Sh)

instance MonadSh OkApp where
  liftSh = lift . liftSh

data RunMode = Display
             | GetOkLocation
             | GetCmd Text
             deriving (Show)

data RunOpts = RunOpts { runMode         :: RunMode
                       , searchAncestors :: Bool
                       } deriving (Show)


runProgram :: RunOpts -> IO ()
runProgram opts = shelly . flip runReaderT opts . printErrors $ go
  where
    go :: OkApp ()
    go = do
      rm <- asks runMode
      case rm of
        Display ->
          echo_n =<< renderStrict . layoutPretty defaultLayoutOptions . render <$> readOkFile

        GetOkLocation ->
          echo_n =<< toTextIgnore . FP.directory <$> getOkFilePath

        GetCmd identifier ->
          echo_n =<< lookupCommand identifier =<< readOkFile

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

  Command :: [Text] -- ^ Command strings
          -> Text -- ^ Alias
          -> Maybe Text -- ^ Doc string
          -> OkDocument Child

deriving instance Show (OkDocument Child)
deriving instance Show (OkDocument Root)
deriving instance Eq (OkDocument Child)
deriving instance Eq (OkDocument Root)


commandStrings :: Bool -> OkDocument a -> [Text]
commandStrings recurse document =
  case document of
    DocumentRoot children _ -> children
                               & filterChildren
                               & concatMap (commandStrings recurse)
    DocumentSection _ children -> children
                                  & filterChildren
                                  & concatMap (commandStrings recurse)
    Command cmdStrs _ _ -> cmdStrs

  where
    filterChildren :: [OkDocument Child] -> [OkDocument Child]
    filterChildren = if recurse then id
                     else mapMaybe (\case c@(Command _ _ _) -> Just c
                                          _ -> Nothing
                                   )

documentChildren :: OkDocument a -> [OkDocument Child]
documentChildren (DocumentRoot children _)    = children
documentChildren (DocumentSection _ children) = children
documentChildren (Command _ _ _)              = []

--- Command Line Opts ---

parseOpts :: IO RunOpts
parseOpts = Opt.execParser $ Opt.info parser desc
  where
    desc =
      Opt.fullDesc
      <> Opt.header "ok -- makefiles for humans"
      <> Opt.failureCode 1

    parser =
      RunOpts
      <$> ( Opt.flag' GetOkLocation ( Opt.long "locate"
                                      <> Opt.short 'l'
                                      <> Opt.help "Return the directory with .ok file (probably use with -s)"
                                    )
            <|> GetCmd <$> Opt.strArgument (Opt.metavar "COMMAND")
            <|> pure Display
          )
      <*> Opt.switch ( Opt.long "search"
                       <> Opt.short 's'
                       <> Opt.help "Search parent directories for .ok file"
                     )
      <**> Opt.helper


--- File Parsing ---

type Parser = Parsec Void Text
type ChildParser = RWST Int (Map Text Text) Int Parser


cmdParser :: ChildParser (OkDocument Child)
cmdParser = do
  a <- aliasParser
  cs <- commandStringParser
  ds <- docStringParser
  lift endOfLine
  if cs == mempty
    then failure Nothing mempty
    else do
    let css = fmap T.strip . T.splitOn ";" $ cs
    alias <- case a of
               Just alias -> pure alias
               Nothing -> do
                 alias <- gets $ T.pack . show
                 modify (+ 1)
                 pure alias
    tell $ Map.singleton alias cs
    return $ Command css alias ds

  where
    commandStringParser = T.strip <$> takeWhileP (Just "command string") (and <$> sequence [(/= '#'), (/= '\n')])

    docStringParser = optional $ do
      takeWhile1P Nothing (== '#')
      ds <- takeWhileP (Just "doc string") (/= '\n')
      return $ T.strip ds

    aliasParser = optional . try $ do
      a <- takeWhile1P (Just "alias") (\c -> isAlphaNum c || c == '-' || c == '_')
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
      indent <- T.length <$> takeWhile1P (Just "heading prefix") (== '#')
      takeWhileP Nothing (== ' ')
      if indent > currLevel
        then lift $ (, indent) <$> takeWhileP (Just "heading") (/= '\n') <* endOfLine
        else failure Nothing mempty


documentParser :: Parser (OkDocument Root)
documentParser = uncurry DocumentRoot <$> evalRWST childrenParser 0 1


parseOkText :: String -> Text -> Either String (OkDocument Root)
parseOkText filename = first errorBundlePretty . parse documentParser filename


endOfLine :: Parser ()
endOfLine = void MP.newline <|> MP.eof


--- Document Rendering ---

type RenderContext = RWS (Int, Int, Int) (Doc AnsiStyle) ()

render :: OkDocument Root -> Doc AnsiStyle
render root@(DocumentRoot topLevelChildren _) =
  snd $ evalRWS (updateContext root root $ go topLevelChildren) (0,0,0) ()

  where
    dsPad = 2
    aliasPad = 1

    go :: [OkDocument Child] -> RenderContext ()
    go elems = do
      case elems of
        [] -> return ()

        Command cmds alias ds : rest -> do
          (dsOffset, aliasOffset, _) <- ask
          let commandPrefix :: Doc AnsiStyle -> Doc AnsiStyle
              commandPrefix doc =
                width (annotate (color Green) $ pretty alias <> ":") (\w -> indent (aliasOffset - w) $ align doc)

              commandSuffix :: Doc AnsiStyle -> Doc AnsiStyle
              commandSuffix doc =
                case ds of
                  Nothing -> doc <> hardline
                  Just dsStr ->
                    width doc (\w -> annotate (color Blue) $
                                indent (dsOffset - w + aliasOffset) $ "#" <+> pretty dsStr)
                    <> hardline

              commandBody :: Doc AnsiStyle
              commandBody =
                cmds
                & fmap pretty
                & intersperse (";" <> hardline)
                & mconcat

          tell $
            commandBody
            & commandPrefix
            & commandSuffix
          go rest

        sec@(DocumentSection title children) : rest -> do
          (_,_,depth) <- ask
          let header = annotate (color Red) $
                       pretty (T.replicate depth "#") <+> pretty title <> hardline
          tell header
          updateContext sec sec $ go children
          go rest

    updateContext :: OkDocument a -> OkDocument b -> RenderContext c -> RenderContext c
    updateContext dsAlignCtx aliasAlignCtx =
      let
        dsOffset = dsAlignCtx
                   & commandStrings False
                   & fmap ((dsPad +) . T.length)
                   & (0 :)
                   & maximum
        aliasOffset = aliasAlignCtx
                      & documentChildren
                      & fmap (\case
                                 Command _ a _ -> T.length a + 1 + aliasPad
                                 _ -> 0
                             )
                      & (2 + aliasPad :)
                      & maximum
      in
        local (\(_,_,depth) -> (dsOffset, aliasOffset, depth+1))


--- File I/O ---

getOkFilePath :: OkApp FilePath
getOkFilePath = pwd >>= checkDir
  where
    checkDir path = do
      let filePath = path </> (".ok" :: FilePath)
      exists <- test_f filePath
      if exists
        then return filePath
        else do
        canRecurse <- asks searchAncestors
        if not canRecurse
          then throwError $ "Couldn't find file " <> show (toTextIgnore filePath)
          else do
          let parent = FP.parent path
          parentExists <- (&&) (parent /= path)
                          <$> test_d parent
          if parentExists
            then checkDir parent
            else throwError $ "Couldn't find an .ok file"


readOkFile :: OkApp (OkDocument Root)
readOkFile = do
  okFile <- getOkFilePath
  liftEither . parseOkText (show okFile) =<< lift (readfile okFile)


--- Command Execution ---

lookupCommand :: Text -> OkDocument Root -> OkApp Text
lookupCommand ref (DocumentRoot _ table) =
  case Map.lookup ref table of
    Nothing  -> throwError $ "Couldn't find command " <> T.unpack ref
    Just cmd -> pure cmd
