{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
  ( Widget, App(..), AttrMap, BrickEvent(..), EventM
  , (<=>), (<+>), txt, halt, attrMap, on, fg
  , defaultMain, showFirstCursor, padBottom, Padding(Max,Pad)
  , padAll, padLeft, nestEventM', nestEventM, modify, gets, get, attrName
  )
import Brick.Widgets.BetterDialog (dialog)
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Edit.EmacsBindings
  ( Editor, renderEditor, handleEditorEvent, getEditContents, editContentsL
  , editorText
  )
import Brick.Widgets.List
  ( List, listMoveDown, listMoveUp, listMoveTo, listSelectedElement
  , listSelectedAttr, list
  )
import Brick.Widgets.List.Utils (listSimpleReplace)
import Graphics.Vty
  (Event(EvKey), Modifier(MCtrl,MMeta), Key(..), defAttr, black, white, green)

import Control.Exception (SomeException, try)
import Control.Monad (msum, when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid (First(..), getFirst)
import qualified Data.Semigroup as Sem
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Zipper (gotoEOL, textZipper)
import qualified Data.Vector as V
import qualified Hledger as HL
import qualified Hledger.Read.JournalReader as HL
import Lens.Micro ((&), (.~), (^.), (%~))
import Lens.Micro.Mtl
import qualified Options.Applicative as OA
import Options.Applicative
  ( ReadM, Parser, value, help, long, metavar, switch, helper, fullDesc, info
  , header, short, (<|>), eitherReader, execParser
  )
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStr, hPutStrLn, stderr)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import Brick.Widgets.CommentDialog
import Brick.Widgets.HelpMessage
import Brick.Widgets.Border.Utils (borderLeft)
import ConfigParser hiding (parseConfigFile)
import DateParser
import Model
import View

import Lens.Micro.TH

import Data.Version (showVersion)
import qualified Paths_hledger_iadd as Paths


data Name = HelpName | ListName | EditorName | CommentName
  deriving (Ord, Show, Eq)

data CommentType = TransactionComment | CurrentComment

instance Show CommentType where
  show TransactionComment = "Transaction comment"
  show CurrentComment = "Comment"

data DialogShown = NoDialog
                 | HelpDialog (HelpWidget Name)
                 | QuitDialog
                 | AbortDialog
                 | CommentDialog CommentType (CommentWidget Name)



data AppState = AppState
  { _asEditor :: Editor Name
  , _asStep :: Step
  , _asJournal :: HL.Journal
  , _asContext :: List Name Text
  , _asSuggestion :: Maybe Text
  , _asMessage :: Text
  , _asFilename :: FilePath
  , _asDateFormat :: DateFormat
  , _asMatchAlgo :: MatchAlgo
  , _asDialog :: DialogShown
  , _asInputHistory :: [Text]
  }

makeLenses ''AppState


myHelpDialog :: DialogShown
myHelpDialog = HelpDialog (helpWidget HelpName bindings)

myCommentDialog :: CommentType -> Text -> DialogShown
myCommentDialog typ comment =
  CommentDialog typ (commentWidget CommentName (T.pack $ show typ) comment)

bindings :: KeyBindings
bindings = KeyBindings
  [ ("Denial",
     [ ("C-c, C-d", "Quit without saving the current transaction")
     , ("Esc", "Abort the current transaction or exit when at toplevel")
     ])
  , ("Anger",
     [ ("F1, Alt-?", "Show help screen")])
  , ("Bargaining",
     [ ("C-n", "Select the next context item")
       , ("C-p", "Select the previous context item")
       , ("Tab", "Insert currently selected answer into text area")
       , ("C-z", "Undo")
       , (";", "Edit comment for current prompt")
       , ("Alt-;", "Edit transaction comment")
       ])
  , ("Acceptance",
     [ ("Ret", "Accept the currently selected answer")
     , ("Alt-Ret", "Accept the current answer verbatim, ignoring selection")
     ])]

draw :: AppState -> [Widget Name]
draw as = case as^.asDialog of
  HelpDialog h -> [renderHelpWidget h, ui]
  QuitDialog -> [quitDialog, ui]
  AbortDialog -> [abortDialog, ui]
  CommentDialog _ c -> [renderCommentWidget c, ui]
  NoDialog -> [ui]

  where ui =  txt "New Transaction:"
          <=> padAll 1 (borderLeft $ padLeft (Pad 1) $ viewState (as^.asStep))
          <=> hBorder
          <=> (viewQuestion (as^.asStep)
               <+> viewSuggestion (as^.asSuggestion)
               <+> txt ": "
               <+> renderEditor True (as^.asEditor))
          <=> hBorder
          <=> expand (viewContext (as^.asContext))
          <=> hBorder
          <=> viewMessage (as^.asMessage)

        quitDialog = dialog "Quit" "Really quit without saving the current transaction? (Y/n)"
        abortDialog = dialog "Abort" "Really abort this transaction (Y/n)"

setComment :: CommentType -> Text -> Step -> Step
setComment TransactionComment = setTransactionComment
setComment CurrentComment     = setCurrentComment

-- TODO Refactor to remove code duplication in individual case statements
event :: BrickEvent Name Event -> EventM Name AppState ()
event (VtyEvent ev) = use asDialog >>= \case
  HelpDialog helpDia -> case ev of
    EvKey key []
      | key `elem` [KChar 'q', KEsc] -> asDialog .= NoDialog
      | otherwise                    -> do
          nestEventM' helpDia (handleHelpEvent ev) >>= assign asDialog . HelpDialog
    _ -> return ()
  QuitDialog -> case ev of
    EvKey key []
      | key `elem` [KChar 'y', KChar 'Y', KEnter] -> halt
      | otherwise -> asDialog .= NoDialog
    _ -> return ()
  AbortDialog -> case ev of
    EvKey key []
      | key `elem` [KChar 'y', KChar 'Y', KEnter] -> do
          asDialog .= NoDialog
          reset
      | otherwise -> asDialog .= NoDialog
    _ -> return ()
  CommentDialog typ dia -> nestEventM dia (handleCommentEvent ev) >>= \case
    (dia', CommentContinue) -> do
      asDialog .= CommentDialog typ dia'
      asStep %= setComment typ (commentDialogComment dia')
    (_, CommentFinished comment) -> do
      asDialog .= NoDialog
      asStep %= setComment typ comment

  NoDialog -> case ev of
    EvKey (KChar 'c') [MCtrl] -> use asStep >>= \case
      DateQuestion _ -> halt
      _              -> asDialog .= QuitDialog
    EvKey (KChar 'd') [MCtrl] -> use asStep >>= \case
      DateQuestion _ -> halt
      _              -> asDialog .= QuitDialog
    EvKey (KChar 'n') [MCtrl] -> do
      asContext %= listMoveDown
      asMessage .= ""
    EvKey KDown [] -> do
      asContext %= listMoveDown
      asMessage .= ""
    EvKey (KChar 'p') [MCtrl] -> do
      asContext %= listMoveUp
      asMessage .= ""
    EvKey KUp [] -> do
      asContext %= listMoveUp
      asMessage .= ""
    EvKey (KChar '\t') [] -> modify insertSelected
    EvKey (KChar ';') [] -> do
      step <- use asStep
      asDialog .= myCommentDialog CurrentComment (getCurrentComment step)
    EvKey (KChar ';') [MMeta] -> do
      step <- use asStep
      asDialog .= myCommentDialog TransactionComment (getTransactionComment step)
    EvKey KEsc [] -> use asStep >>= \case
      DateQuestion _ -> do
        t <- gets editText
        if T.null t then halt else reset
      _ -> asDialog .= AbortDialog
    EvKey (KChar 'z') [MCtrl] -> doUndo
    EvKey KEnter [MMeta] -> doNextStep False
    EvKey KEnter [] -> doNextStep True
    EvKey (KFun 1) [] -> asDialog .= myHelpDialog
    EvKey (KChar '?') [MMeta] -> asDialog .= myHelpDialog >> asMessage .= "Help"
    _ -> do
      zoom asEditor $ handleEditorEvent ev
      setContext
event _ = return ()

reset :: EventM n AppState ()
reset = do
  as <- get
  sugg <- liftIO $ suggest (as^.asJournal) (as^.asDateFormat) (DateQuestion "")
  
  asStep .= DateQuestion ""
  asEditor %= clearEdit
  asContext .= ctxList V.empty
  asSuggestion .= sugg
  asMessage .= "Transaction aborted"


setContext :: EventM n AppState ()
setContext = do
  as <- get
  newCtx <- liftIO $ context (as^.asJournal) (as^.asMatchAlgo) (as^.asDateFormat) (editText as) (as^.asStep)
  asContext %= listSimpleReplace (V.fromList newCtx)
      

editText :: AppState -> Text
editText = T.concat . getEditContents . _asEditor

-- | Add a tranaction at the end of a journal
--
-- Hledgers `HL.addTransaction` adds it to the beginning, but our suggestion
-- system expects newer transactions to be at the end.
addTransactionEnd :: HL.Transaction -> HL.Journal -> HL.Journal
addTransactionEnd t j = j { HL.jtxns = HL.jtxns j ++ [t] }

doNextStep :: Bool -> EventM n AppState ()
doNextStep useSelected = do
  as <- get
  let inputText = editText as
      name = fromMaybe (Left inputText) $
               msum [ Right <$> if useSelected then snd <$> listSelectedElement (as^.asContext) else Nothing
                    , Left <$> asMaybe (editText as)
                    , Left <$> as^.asSuggestion
                    ]
  s <- liftIO $ nextStep (as^.asJournal) (as^.asDateFormat) name (as^.asStep)
  case s of
    Left err -> asMessage .= err
    Right (Finished trans) -> do
      liftIO $ addToJournal trans (as^.asFilename)
      sugg <- liftIO $ suggest (as^.asJournal) (as^.asDateFormat) (DateQuestion "")
      asStep .= DateQuestion ""
      asJournal %= addTransactionEnd trans
      asEditor %= clearEdit
      asContext .= ctxList V.empty
      asSuggestion .= sugg
      asMessage .= "Transaction written to journal file"
      asDialog .= NoDialog
      asInputHistory .= []
    Right (Step s') -> do
      sugg <- liftIO $ suggest (as^.asJournal) (as^.asDateFormat) s'
      ctx' <- ctxList . V.fromList <$> liftIO (context (as^.asJournal) (as^.asMatchAlgo) (as^.asDateFormat) "" s')
      asStep .= s'
      asEditor %= clearEdit
      asContext .= ctx'
      asSuggestion .= sugg
      asMessage .= ""
      -- Adhere to the 'undo' behaviour: when in the final
      -- confirmation question, 'undo' jumps back to the last amount
      -- question instead of to the last account question. So do not
      -- save the last empty account answer which indicates the end
      -- of the transaction.
      -- Furthermore, don't save the input if the FinalQuestion is
      -- answered by 'n' (for no).
      case (as^.asStep, s') of
        (FinalQuestion _ _, _) -> return ()
        (_, FinalQuestion _ _) -> return ()
        _                      -> asInputHistory %= (inputText :)

doUndo :: EventM n AppState ()
doUndo = use asStep >>= \s -> case undo s of
  Left msg -> asMessage .= "Undo failed: " <> msg
  Right step -> do
    as <- get
    let (lastInput,historyTail) =
          case as^.asInputHistory of
            x:t -> (x,t)
            [] -> ("",[])

    sugg <- liftIO $ suggest (as^.asJournal) (as^.asDateFormat) step
    asStep .= step
    asEditor %= setEdit lastInput
    asSuggestion .= sugg
    asMessage .= "Undo."
    asInputHistory .= historyTail
    setContext

insertSelected :: AppState -> AppState
insertSelected as = case listSelectedElement (as^.asContext) of
  Nothing -> as
  Just (_, line) -> as & asEditor %~ setEdit line


asMaybe :: Text -> Maybe Text
asMaybe t
  | T.null t  = Nothing
  | otherwise = Just t

attrs :: AttrMap
attrs = attrMap defAttr
  [ (listSelectedAttr, black `on` white)
  , (helpAttr <> attrName "title", fg green)
  ]

clearEdit :: Editor n -> Editor n
clearEdit = setEdit ""

setEdit :: Text -> Editor n -> Editor n
setEdit content edit = edit & editContentsL .~ zipper
  where zipper = gotoEOL (textZipper [content] (Just 1))

addToJournal :: HL.Transaction -> FilePath -> IO ()
addToJournal trans path = appendFile path (T.unpack $ moveEmptyLine $ HL.showTransaction trans)
  where
    -- showTransactionUnelided adds an empty line to the end of the transaction. We want
    -- the empty line to be at the start instead, to allow it to be added to a
    -- journal that doesn't end with a newline.
    moveEmptyLine :: Text -> Text
    moveEmptyLine = T.unlines . ("":) . init . T.lines

--------------------------------------------------------------------------------
-- Command line and config parsing
--------------------------------------------------------------------------------

data CommonOptions f = CommonOptions
  { optLedgerFile :: f FilePath
  , optDateFormat :: f String
  , optMatchAlgo  :: f MatchAlgo
  }

instance Sem.Semigroup (CommonOptions Maybe) where
  (<>) opt1 opt2 =
    let opt1' = optNatTrans First opt1
        opt2' = optNatTrans First opt2
    in optNatTrans getFirst $ CommonOptions
       { optLedgerFile = optLedgerFile opt1' <> optLedgerFile opt2'
       , optDateFormat = optDateFormat opt1' <> optDateFormat opt2'
       , optMatchAlgo = optMatchAlgo opt1' <> optMatchAlgo opt2'
       }

instance Monoid (CommonOptions Maybe) where
  mappend = (Sem.<>)
  mempty = CommonOptions Nothing Nothing Nothing

optNatTrans :: (forall a. f a -> g a) -> CommonOptions f -> CommonOptions g
optNatTrans nat opts = CommonOptions
  { optLedgerFile = nat $ optLedgerFile opts
  , optDateFormat = nat $ optDateFormat opts
  , optMatchAlgo = nat $ optMatchAlgo opts
  }

optFromJust :: CommonOptions Identity -> CommonOptions Maybe -> CommonOptions Identity
optFromJust def opts =
  optNatTrans (Identity . fromJust) ( opts <> optNatTrans (Just . runIdentity) def)

data CmdLineOptions = CmdLineOptions
  { cmdCommon :: CommonOptions Maybe
  , cmdDumpConfig :: Bool
  , cmdVersion :: Bool
  }

data ConfOptions = ConfOptions { confCommon :: CommonOptions Maybe }

defaultOptions :: FilePath -> CommonOptions Identity
defaultOptions home = CommonOptions
  { optLedgerFile = Identity (ledgerPath home)
  , optDateFormat = Identity "[[%y/]%m/]%d"
  , optMatchAlgo = Identity Substrings
  }

ledgerPath :: FilePath -> FilePath
ledgerPath home = home <> "/.hledger.journal"

configPath :: IO FilePath
configPath = getUserConfigFile "hledger-iadd" "config.conf"

-- | Megaparsec parser for MatchAlgo, used for config file parsing
parseMatchAlgo :: OParser MatchAlgo
parseMatchAlgo =  (P.string "fuzzy" *> pure Fuzzy)
              <|> (P.string "substrings" *> pure Substrings)

-- | ReadM parser for MatchAlgo, used for command line option parsing
readMatchAlgo :: ReadM MatchAlgo
readMatchAlgo = eitherReader reader
  where
    reader str
      | str == "fuzzy" = return Fuzzy
      | str == "substrings" = return Substrings
      | otherwise = Left "Expected \"fuzzy\" or \"substrings\""

-- | Parser for our config file
confParser :: CommonOptions Identity -> OptParser ConfOptions
confParser def = fmap ConfOptions $ CommonOptions
  -- TODO Convert leading tilde to home
  <$> (Just <$> option "file" (runIdentity $ optLedgerFile def) "Path to the journal file")
  <*> (Just <$> option "date-format" (runIdentity $ optDateFormat def) "Format used to parse dates")
  <*> (Just <$> customOption "completion-engine" matchAlgo (T.toLower $ T.pack $ show matchAlgo)
      ( "Algorithm used to find completions for account names. Possible values are:\n"
      <> "  - substrings: Every word in the search string has to occur somewhere in the account name\n"
      <> "  - fuzzy: All letters from the search string have to appear in the name in the same order"
      )
      "string"
      parseMatchAlgo
      )

  where matchAlgo = runIdentity (optMatchAlgo def)

-- | IO Action to read and parse config file
parseConfigFile :: IO ConfOptions
parseConfigFile = do
  path <- configPath
  home <- getHomeDirectory
  let def = defaultOptions home

  try (T.readFile path) >>= \case
    Left (_ :: SomeException) -> return (parserDefault $ confParser def)
    Right res -> case parseConfig path res (confParser def) of
      Left err -> do
        putStr (P.errorBundlePretty err)
        exitFailure
      Right res' -> return res'

-- | command line option parser
cmdOptionParser :: Parser CmdLineOptions
cmdOptionParser = CmdLineOptions
  <$> (CommonOptions
       <$> OA.option (Just <$> OA.str)
           (  long "file"
           <> short 'f'
           <> metavar "FILE"
           <> value Nothing
           <> help "Path to the journal file"
           )
       <*> OA.option (Just <$> OA.str)
             (  long "date-format"
             <> metavar "FORMAT"
             <> value Nothing
             <> help "Format used to parse dates"
             )
      <*> OA.option (Just <$> readMatchAlgo)
            (  long "completion-engine"
            <> metavar "ENGINE"
            <> value Nothing
            <> help "Algorithm for account name completion. Possible values: \"fuzzy\", \"substrings\"")
      )
  <*> switch
        ( long "dump-default-config"
       <> help "Print an example configuration file to stdout and exit"
        )
  <*> switch
        ( long "version"
       <> help "Print version number and exit"
        )

parseEnvVariables :: IO (CommonOptions Maybe)
parseEnvVariables = do
  maybeFilePath <- lookupEnv "LEDGER_FILE"
  return mempty
    { optLedgerFile = maybeFilePath }

main :: IO ()
main = do
  home <- getHomeDirectory
  path <- configPath
  let defOpts = defaultOptions home

  cmdOpts <- execParser $ info (helper <*> cmdOptionParser) $
               fullDesc <> header "A terminal UI as drop-in replacement for hledger add."

  when (cmdVersion cmdOpts) $ do
    putStrLn $ "This is hledger-iadd version " <> showVersion Paths.version
    exitSuccess

  when (cmdDumpConfig cmdOpts) $ do
    T.putStrLn $ "# Write this to " <> T.pack path <> "\n"
    T.putStrLn (parserExample $ confParser defOpts)
    exitSuccess

  confOpts <- parseConfigFile

  envOpts <- parseEnvVariables

  -- The order of precedence here is:
  -- arguments > environment > config file
  let opts = optFromJust defOpts $ cmdCommon cmdOpts <> envOpts <> confCommon confOpts

  date <- case parseDateFormat (T.pack $ runIdentity $ optDateFormat opts) of
    Left err -> do
      hPutStr stderr "Could not parse date format: "
      T.hPutStr stderr err
      exitFailure
    Right res -> return res

  let path = runIdentity $ optLedgerFile opts
  journalContents <- T.readFile path

  runExceptT (HL.parseAndFinaliseJournal HL.journalp HL.definputopts path journalContents) >>= \case
    Left err -> hPutStrLn stderr err >> exitFailure
    Right journal -> do
      let edit = editorText EditorName (txt . T.concat) (Just 1) ""

      sugg <- suggest journal date (DateQuestion "")

      let welcome = "Welcome! Press F1 (or Alt-?) for help. Exit with Ctrl-d."
          matchAlgo = runIdentity $ optMatchAlgo opts
          as = AppState edit (DateQuestion "") journal (ctxList V.empty) sugg welcome path date matchAlgo NoDialog []

      void $ defaultMain app as

    where app = App { appDraw = draw
                    , appChooseCursor = showFirstCursor
                    , appHandleEvent = event
                    , appAttrMap = const attrs
                    , appStartEvent = return ()
                    } :: App AppState Event Name

expand :: Widget n -> Widget n
expand = padBottom Max

ctxList :: V.Vector e -> List Name e
ctxList v = (if V.null v then id else listMoveTo 0) $ list ListName v 1
