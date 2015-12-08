{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Brick.Widgets.List.Utils
import           Graphics.Vty

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Zipper
import qualified Data.Vector as V
import qualified Hledger as HL
import qualified Hledger.Read.JournalReader as HL
import           Options.Applicative hiding (str)
import           System.Directory
import           System.Exit
import           System.IO

import           DateParser
import           Model
import           View

data AppState = AppState
  { asEditor :: Editor
  , asStep :: Step
  , asJournal :: HL.Journal
  , asContext :: List Text
  , asSuggestion :: Maybe Text
  , asMessage :: Text
  , asFilename :: FilePath
  , asDateFormat :: DateFormat
  }


draw :: AppState -> [Widget]
draw as = [ui]
  where ui =  viewState (asStep as)
          <=> hBorder
          <=> (viewQuestion (asStep as)
               <+> viewSuggestion (asSuggestion as)
               <+> txt ": "
               <+> renderEditor (asEditor as))
          <=> hBorder
          <=> expand (viewContext (asContext as))
          <=> hBorder
          <=> txt (asMessage as <> " ") -- TODO Add space only if message is empty

event :: AppState -> Event -> EventM (Next AppState)
event as ev = case ev of
  EvKey (KChar 'c') [MCtrl] -> halt as
  EvKey (KChar 'n') [MCtrl] -> continue as { asContext = listMoveDown $ asContext as
                                           , asMessage = ""}
  EvKey (KChar 'p') [MCtrl] -> continue as { asContext = listMoveUp $ asContext as
                                           , asMessage = ""}
  EvKey (KChar '\t') [] -> continue (insertSelected as)
  EvKey KEsc [] -> liftIO (reset as) >>= continue
  EvKey (KChar 'z') [MCtrl] -> liftIO (doUndo as) >>= continue
  EvKey KEnter [MMeta] -> liftIO (doNextStep False as) >>= continue
  EvKey KEnter [] -> liftIO (doNextStep True as) >>= continue
  _ -> (AppState <$> handleEvent ev (asEditor as)
                 <*> return (asStep as)
                 <*> return (asJournal as)
                 <*> return (asContext as)
                 <*> return (asSuggestion as)
                 <*> return ""
                 <*> return (asFilename as))
                 <*> return (asDateFormat as)
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
  [ (listSelectedAttr, black `on` white) ]

clearEdit :: Editor -> Editor
clearEdit = setEdit ""

setEdit :: Text -> Editor -> Editor
setEdit content edit = edit & editContentsL .~ zipper
  where zipper = gotoEOL (stringZipper [T.unpack content] (Just 1))

addToJournal :: HL.Transaction -> FilePath -> IO ()
addToJournal trans path = appendFile path (show trans)


ledgerPath :: FilePath -> FilePath
ledgerPath home = home <> "/.hledger.journal"

data Options = Options
  { optLedgerFile :: FilePath
  , optDateFormat :: String
  }

optionParser :: FilePath -> Parser Options
optionParser home = Options
  <$> strOption
        (  long "file"
        <> short 'f'
        <> metavar "FILE"
        <> value (ledgerPath home)
        <> help "Path to the journal file"
        )
  <*> strOption
        (  long "date-format"
        <> metavar "FORMAT"
        <> value "%d[.[%m[.[%y]]]]"
        <> help "Format used to parse dates"
        )

main :: IO ()
main = do
  home <- getHomeDirectory

  opts <- execParser $ info (helper <*> optionParser home) $
             fullDesc <> header "A terminal UI as drop-in replacement for hledger add."

  date <- case parseDateFormat (T.pack $ optDateFormat opts) of
    Left err -> do
      hPutStr stderr "Could not parse date format: "
      T.hPutStrLn stderr err
      exitWith (ExitFailure 1)
    Right res -> return res

  let path = optLedgerFile opts
  journalContents <- readFile path
  Right journal <- runExceptT $ HL.parseAndFinaliseJournal HL.journalp True path journalContents

  let edit = editor "Edit" (str . concat) (Just 1) ""

  sugg <- suggest journal date DateQuestion

  let as = AppState edit DateQuestion journal (ctxList V.empty) sugg "Welcome" path date

  void $ defaultMain app as

  where app = App { appDraw = draw
                  , appChooseCursor = showFirstCursor
                  , appHandleEvent = event
                  , appAttrMap = const attrs
                  , appLiftVtyEvent = id
                  , appStartEvent = return
                  } :: App AppState Event

expand :: Widget -> Widget
expand = padBottom Max

ctxList :: V.Vector e -> List e
ctxList v = (if V.null v then id else listMoveTo 0) $ list "Context" v 1
