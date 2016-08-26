{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.BetterDialog
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Brick.Widgets.List.Utils
import           Graphics.Vty hiding (parseConfigFile, parseConfig)

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.HashMap.Strict as HM
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
import           Options.Applicative hiding (str, option)
import           System.Directory
import           System.Environment.XDG.BaseDir
import           System.Exit
import           System.IO

import           Brick.Widgets.HelpMessage
import           DateParser
import           ConfigParser hiding (parseConfigFile)
import           Model
import           View

data AppState = AppState
  { asEditor :: Editor Name
  , asStep :: Step
  , asJournal :: HL.Journal
  , asContext :: List Name Text
  , asSuggestion :: Maybe Text
  , asMessage :: Text
  , asFilename :: FilePath
  , asDateFormat :: DateFormat
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
     [ ("C-c", "Quit without saving the current transaction")
     , ("Esc", "Abort the current transaction")
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
          <=> txt (asMessage as <> " ") -- TODO Add space only if message is empty

        quitDialog = dialog "Quit" "Really quit without saving the current transaction? (Y/n)"
        abortDialog = dialog "Abort" "Really abort this transaction (Y/n)"

event :: AppState -> Event -> EventM Name (Next AppState)
event as ev = case asDialog as of
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
      | asStep as == DateQuestion -> liftIO (reset as) >>= continue
      | otherwise -> continue as { asDialog = AbortDialog }
    EvKey (KChar 'z') [MCtrl] -> liftIO (doUndo as) >>= continue
    EvKey KEnter [MMeta] -> liftIO (doNextStep False as) >>= continue
    EvKey KEnter [] -> liftIO (doNextStep True as) >>= continue
    EvKey (KFun 1) [] -> continue as { asDialog = myHelpDialog }
    EvKey (KChar '?') [MMeta] -> continue as { asDialog = myHelpDialog, asMessage = "Help" }
    EvKey (KChar 'u') [MCtrl] -> continue as { asEditor = clearEdit (asEditor as) }
    _ -> (AppState <$> handleEditorEvent ev (asEditor as)
                   <*> return (asStep as)
                   <*> return (asJournal as)
                   <*> return (asContext as)
                   <*> return (asSuggestion as)
                   <*> return ""
                   <*> return (asFilename as))
                   <*> return (asDateFormat as)
                   <*> return NoDialog
         >>= liftIO . setContext >>= continue

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
         context (asJournal as) (asDateFormat as) (editText as) (asStep as)
  return as { asContext = ctx }

editText :: AppState -> Text
editText = T.pack . concat . getEditContents . asEditor

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
        , asJournal = HL.addTransaction trans (asJournal  as)
        , asEditor = clearEdit (asEditor as)
        , asContext = ctxList V.empty
        , asSuggestion = sugg
        , asMessage = "Transaction written to journal file"
        , asFilename = asFilename as
        , asDateFormat = asDateFormat as
        , asDialog = NoDialog
        }
    Right (Step s') -> do
      sugg <- suggest (asJournal as) (asDateFormat as) s'
      ctx' <- ctxList . V.fromList <$> context (asJournal as) (asDateFormat as) "" s'
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

clearEdit :: Editor n -> Editor n
clearEdit = setEdit ""

setEdit :: Text -> Editor n -> Editor n
setEdit content edit = edit & editContentsL .~ zipper
  where zipper = gotoEOL (stringZipper [T.unpack content] (Just 1))

addToJournal :: HL.Transaction -> FilePath -> IO ()
addToJournal trans path = appendFile path (show trans)


ledgerPath :: FilePath -> FilePath
ledgerPath home = home <> "/.hledger.journal"

configPath :: IO FilePath
configPath = getUserConfigFile "hledger-iadd" "config.conf"

data Options = Options
  { optLedgerFile :: FilePath
  , optDateFormat :: String
  }

confParser :: FilePath -> OptParser Options
confParser home = Options
  <$> option "file" (ledgerPath home) "Path to the journal file"
  <*> option "date-format" "[[%y/]%m/]%d" "Format used to parse dates"

parseConfigFile :: IO Options
parseConfigFile = do
  path <- configPath
  home <- getHomeDirectory

  try (T.readFile path) >>= \case
    Left (_ :: SomeException) -> return (parserDefault $ confParser home)
    Right res -> case parseConfig path res (confParser home) of
      Left err -> do
        print err
        exitFailure
      Right res' -> return res'

optionParser :: Options -> Parser Options
optionParser def = Options
  <$> strOption
        (  long "file"
        <> short 'f'
        <> metavar "FILE"
        <> value (optLedgerFile def)
        <> help "Path to the journal file"
        )
  <*> strOption
        (  long "date-format"
        <> metavar "FORMAT"
        <> value (optDateFormat def)
        <> help "Format used to parse dates"
        )

main :: IO ()
main = do
  opts1 <- parseConfigFile

  opts <- execParser $ info (helper <*> optionParser opts1) $
            fullDesc <> header "A terminal UI as drop-in replacement for hledger add."

  date <- case parseDateFormat (T.pack $ optDateFormat opts) of
    Left err -> do
      hPutStr stderr "Could not parse date format: "
      T.hPutStrLn stderr err
      exitWith (ExitFailure 1)
    Right res -> return res

  let path = optLedgerFile opts
  journalContents <- readFile path

  runExceptT (HL.parseAndFinaliseJournal HL.journalp True path journalContents) >>= \case
    Left err -> hPutStrLn stderr err >> exitFailure
    Right journal -> do
      let edit = editor EditorName (str . concat) (Just 1) ""

      sugg <- suggest journal date DateQuestion

      let welcome = "Welcome. Press F1 (or Alt-?) for help."
          as = AppState edit DateQuestion journal (ctxList V.empty) sugg welcome path date NoDialog

      void $ defaultMain app as

    where app = App { appDraw = draw
                    , appChooseCursor = showFirstCursor
                    , appHandleEvent = event
                    , appAttrMap = const attrs
                    , appLiftVtyEvent = id
                    , appStartEvent = return
                    } :: App AppState Event Name

expand :: Widget n -> Widget n
expand = padBottom Max

ctxList :: V.Vector e -> List Name e
ctxList v = (if V.null v then id else listMoveTo 0) $ list ListName v 1
