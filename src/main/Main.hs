{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.BetterDialog
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Brick.Widgets.List.Utils
import           Graphics.Vty hiding (parseConfigFile, (<|>))

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Zipper
import qualified Data.Vector as V
import qualified Hledger as HL
import qualified Hledger.Read.JournalReader as HL
import           Lens.Micro
import qualified Options.Applicative as OA
import           Options.Applicative hiding (str, option)
import           System.Directory
import           System.Environment.XDG.BaseDir
import           System.Exit
import           System.IO
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Text as P

import           Brick.Widgets.HelpMessage
import           DateParser
import           ConfigParser hiding (parseConfigFile)
import           Model
import           View

data AppState = AppState
  { asEditor :: Editor Text Name
  , asStep :: Step
  , asJournal :: HL.Journal
  , asContext :: List Name Text
  , asSuggestion :: Maybe Text
  , asMessage :: Text
  , asFilename :: FilePath
  , asDateFormat :: DateFormat
  , asMatchAlgo :: MatchAlgo
  , asDialog :: DialogShown
  }

data Name = HelpName | ListName | EditorName
  deriving (Ord, Show, Eq)

data DialogShown = NoDialog | HelpDialog (HelpWidget Name) | QuitDialog | AbortDialog

myHelpDialog :: DialogShown
myHelpDialog = HelpDialog (helpWidget HelpName bindings)

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
       ])
  , ("Acceptance",
     [ ("Ret", "Accept the currently selected answer")
     , ("Alt-Ret", "Accept the current answer verbatim, ignoring selection")
     ])]

draw :: AppState -> [Widget Name]
draw as = case asDialog as of
  HelpDialog h -> [renderHelpWidget h, ui]
  QuitDialog -> [quitDialog, ui]
  AbortDialog -> [abortDialog, ui]
  NoDialog -> [ui]

  where ui =  viewState (asStep as)
          <=> hBorder
          <=> (viewQuestion (asStep as)
               <+> viewSuggestion (asSuggestion as)
               <+> txt ": "
               <+> renderEditor True (asEditor as))
          <=> hBorder
          <=> expand (viewContext (asContext as))
          <=> hBorder
          <=> viewMessage (asMessage as)

        quitDialog = dialog "Quit" "Really quit without saving the current transaction? (Y/n)"
        abortDialog = dialog "Abort" "Really abort this transaction (Y/n)"

-- TODO Refactor to remove code duplication in individual case statements
event :: AppState -> BrickEvent Name Event -> EventM Name (Next AppState)
event as (VtyEvent ev) = case asDialog as of
  HelpDialog helpDia -> case ev of
    EvKey key []
      | key `elem` [KChar 'q', KEsc] -> continue as { asDialog = NoDialog }
      | otherwise                    -> do
          helpDia' <- handleHelpEvent helpDia ev
          continue as { asDialog = HelpDialog helpDia' }
    _ -> continue as
  QuitDialog -> case ev of
    EvKey key []
      | key `elem` [KChar 'y', KEnter] -> halt as
      | otherwise -> continue as { asDialog = NoDialog }
    _ -> continue as
  AbortDialog -> case ev of
    EvKey key []
      | key `elem` [KChar 'y', KEnter] ->
        liftIO (reset as { asDialog = NoDialog }) >>= continue
      | otherwise -> continue as { asDialog = NoDialog }
    _ -> continue as
  NoDialog -> case ev of
    EvKey (KChar 'c') [MCtrl]
      | asStep as == DateQuestion -> halt as
      | otherwise -> continue as { asDialog = QuitDialog }
    EvKey (KChar 'd') [MCtrl]
      | asStep as == DateQuestion -> halt as
      | otherwise -> continue as { asDialog = QuitDialog }
    EvKey (KChar 'n') [MCtrl] -> continue as { asContext = listMoveDown $ asContext as
                                             , asMessage = ""}
    EvKey KDown [] -> continue as { asContext = listMoveDown $ asContext as
                                  , asMessage = ""}
    EvKey (KChar 'p') [MCtrl] -> continue as { asContext = listMoveUp $ asContext as
                                             , asMessage = ""}
    EvKey KUp [] -> continue as { asContext = listMoveUp $ asContext as
                               , asMessage = ""}
    EvKey (KChar '\t') [] -> continue (insertSelected as)
    EvKey KEsc []
      | asStep as == DateQuestion && T.null (editText as) -> halt as
      | asStep as == DateQuestion -> liftIO (reset as) >>= continue
      | otherwise -> continue as { asDialog = AbortDialog }
    EvKey (KChar 'z') [MCtrl] -> liftIO (doUndo as) >>= continue
    EvKey KEnter [MMeta] -> liftIO (doNextStep False as) >>= continue
    EvKey KEnter [] -> liftIO (doNextStep True as) >>= continue
    EvKey (KFun 1) [] -> continue as { asDialog = myHelpDialog }
    EvKey (KChar '?') [MMeta] -> continue as { asDialog = myHelpDialog, asMessage = "Help" }
    _ -> (AppState <$> handleEditorEvent ev (asEditor as)
                   <*> return (asStep as)
                   <*> return (asJournal as)
                   <*> return (asContext as)
                   <*> return (asSuggestion as)
                   <*> return ""
                   <*> return (asFilename as))
                   <*> return (asDateFormat as)
                   <*> return (asMatchAlgo as)
                   <*> return NoDialog
         >>= liftIO . setContext >>= continue
event as _ = continue as

reset :: AppState -> IO AppState
reset as = do
  sugg <- suggest (asJournal as) (asDateFormat as) DateQuestion
  return as
    { asStep = DateQuestion
    , asEditor = clearEdit (asEditor as)
    , asContext = ctxList V.empty
    , asSuggestion = sugg
    , asMessage = "Transaction aborted"
    }

setContext :: AppState -> IO AppState
setContext as = do
  ctx <- flip listSimpleReplace (asContext as) . V.fromList <$>
         context (asJournal as) (asMatchAlgo as) (asDateFormat as) (editText as) (asStep as)
  return as { asContext = ctx }

editText :: AppState -> Text
editText = T.concat . getEditContents . asEditor

-- | Add a tranaction at the end of a journal
--
-- Hledgers `HL.addTransaction` adds it to the beginning, but our suggestion
-- system expects newer transactions to be at the end.
addTransactionEnd :: HL.Transaction -> HL.Journal -> HL.Journal
addTransactionEnd t j = j { HL.jtxns = HL.jtxns j ++ [t] }

doNextStep :: Bool -> AppState -> IO AppState
doNextStep useSelected as = do
  let name = fromMaybe (Left $ editText as) $
               msum [ Right <$> if useSelected then snd <$> listSelectedElement (asContext as) else Nothing
                    , Left <$> asMaybe (editText as)
                    , Left <$> asSuggestion as
                    ]
  s <- nextStep (asJournal as) (asDateFormat as) name (asStep as)
  case s of
    Left err -> return as { asMessage = err }
    Right (Finished trans) -> do
      liftIO $ addToJournal trans (asFilename as)
      sugg <- suggest (asJournal as) (asDateFormat as) DateQuestion
      return AppState
        { asStep = DateQuestion
        , asJournal = addTransactionEnd trans (asJournal  as)
        , asEditor = clearEdit (asEditor as)
        , asContext = ctxList V.empty
        , asSuggestion = sugg
        , asMessage = "Transaction written to journal file"
        , asFilename = asFilename as
        , asDateFormat = asDateFormat as
        , asMatchAlgo = asMatchAlgo as
        , asDialog = NoDialog
        }
    Right (Step s') -> do
      sugg <- suggest (asJournal as) (asDateFormat as) s'
      ctx' <- ctxList . V.fromList <$> context (asJournal as) (asMatchAlgo as) (asDateFormat as) "" s'
      return as { asStep = s'
                , asEditor = clearEdit (asEditor as)
                , asContext = ctx'
                , asSuggestion = sugg
                , asMessage = ""
                }

doUndo :: AppState -> IO AppState
doUndo as = case undo (asStep as) of
  Left msg -> return as { asMessage = "Undo failed: " <> msg }
  Right step -> do
    sugg <- suggest (asJournal as) (asDateFormat as) step
    setContext $ as { asStep = step
                    , asEditor = clearEdit (asEditor as)
                    , asSuggestion = sugg
                    , asMessage = "Undo."
                    }

insertSelected :: AppState -> AppState
insertSelected as = case listSelectedElement (asContext as) of
  Nothing -> as
  Just (_, line) -> as { asEditor = setEdit line (asEditor as) }


asMaybe :: Text -> Maybe Text
asMaybe t
  | T.null t  = Nothing
  | otherwise = Just t

attrs :: AttrMap
attrs = attrMap defAttr
  [ (listSelectedAttr, black `on` white)
  , (helpAttr <> "title", fg green)
  ]

clearEdit :: Editor Text n -> Editor Text n
clearEdit = setEdit ""

setEdit :: Text -> Editor Text n -> Editor Text n
setEdit content edit = edit & editContentsL .~ zipper
  where zipper = gotoEOL (textZipper [content] (Just 1))

addToJournal :: HL.Transaction -> FilePath -> IO ()
addToJournal trans path = appendFile path (HL.showTransaction trans)

--------------------------------------------------------------------------------
-- Command line and config parsing
--------------------------------------------------------------------------------

data CommonOptions f = CommonOptions
  { optLedgerFile :: f FilePath
  , optDateFormat :: f String
  , optMatchAlgo  :: f MatchAlgo
  }

instance Monoid (CommonOptions Maybe) where
  mappend opt1 opt2 =
    let opt1' = optNatTrans First opt1
        opt2' = optNatTrans First opt2
    in optNatTrans getFirst $ CommonOptions
       { optLedgerFile = optLedgerFile opt1' <> optLedgerFile opt2'
       , optDateFormat = optDateFormat opt1' <> optDateFormat opt2'
       , optMatchAlgo = optMatchAlgo opt1' <> optMatchAlgo opt2'
       }

  mempty = CommonOptions Nothing Nothing Nothing

optNatTrans :: Functor f => (forall a. f a -> g a) -> CommonOptions f -> CommonOptions g
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
parseMatchAlgo :: P.Parser MatchAlgo
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
        putStr (show err)
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

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  home <- getHomeDirectory
  path <- configPath
  let defOpts = defaultOptions home

  cmdOpts <- execParser $ info (helper <*> cmdOptionParser) $
               fullDesc <> header "A terminal UI as drop-in replacement for hledger add."

  when (cmdDumpConfig cmdOpts) $ do
    T.putStrLn $ "# Write this to " <> T.pack path <> "\n"
    T.putStrLn (parserExample $ confParser defOpts)
    exitSuccess

  confOpts <- parseConfigFile

  let opts = optFromJust defOpts $ cmdCommon cmdOpts <> confCommon confOpts

  date <- case parseDateFormat (T.pack $ runIdentity $ optDateFormat opts) of
    Left err -> do
      hPutStr stderr "Could not parse date format: "
      T.hPutStr stderr err
      exitWith (ExitFailure 1)
    Right res -> return res

  let path = runIdentity $ optLedgerFile opts
  journalContents <- T.readFile path

  runExceptT (HL.parseAndFinaliseJournal HL.journalp True path journalContents) >>= \case
    Left err -> hPutStrLn stderr err >> exitFailure
    Right journal -> do
      let edit = editor EditorName (txt . T.concat) (Just 1) ""

      sugg <- suggest journal date DateQuestion

      let welcome = "Welcome! Press F1 (or Alt-?) for help. Exit with Ctrl-d."
          matchAlgo = runIdentity $ optMatchAlgo opts
          as = AppState edit DateQuestion journal (ctxList V.empty) sugg welcome path date matchAlgo NoDialog

      void $ defaultMain app as

    where app = App { appDraw = draw
                    , appChooseCursor = showFirstCursor
                    , appHandleEvent = event
                    , appAttrMap = const attrs
                    , appStartEvent = return
                    } :: App AppState Event Name

expand :: Widget n -> Widget n
expand = padBottom Max

ctxList :: V.Vector e -> List Name e
ctxList v = (if V.null v then id else listMoveTo 0) $ list ListName v 1
