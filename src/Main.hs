{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Brick.Widgets.List.Utils

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Default
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Zipper
import           Data.Time hiding (parseTime)
import qualified Data.Vector as V
import           Graphics.Vty.Input.Events
import qualified Hledger as HL
import qualified Hledger.Read.JournalReader as HL
import           System.Environment

import           Model
import           View

data AppState = AppState
  { asEditor :: Editor
  , asStep :: Step
  , asJournal :: HL.Journal
  , asContext :: List Text
  }


draw :: AppState -> [Widget]
draw as = [ui]
  where ui =  viewState (asStep as)
          <=> hBorder
          <=> (viewQuestion (asStep as) <+> renderEditor (asEditor as))
          <=> hBorder
          <=> expand (viewContext (asContext as))
          <=> hBorder
          <=> str "A message area"

event :: AppState -> Event -> EventM (Next AppState)
event as ev = case ev of
  EvKey KEsc [] -> halt as
  EvKey KEnter [] -> liftIO (doNextStep as) >>= continue
  _ -> setContext <$>
       (AppState <$> handleEvent ev (asEditor as)
                 <*> return (asStep as)
                 <*> return (asJournal as)
                 <*> return (asContext as))
       >>= continue

setContext as = as { asContext = flip listSimpleReplace (asContext as) $ V.fromList $
  context (asJournal as) (editText as) (asStep as) }

editText = T.pack . concat . getEditContents . asEditor

doNextStep as = do
  let name = fromMaybe (editText as) (snd <$> listSelectedElement (asContext as))
  s' <- nextStep name (asStep as)
  let ctx' = ctxList $ V.fromList $ context (asJournal as) "" s'
  return as { asStep = s'
            , asEditor = clearEdit (asEditor as)
            , asContext = ctx'
            }

clearEdit edit = edit & editContentsL .~ stringZipper [""] (Just 1)

ledgerPath home = home <> "/.hledger.journal"

main :: IO ()
main = do
  home <- getEnv "HOME" -- FIXME
  let path = ledgerPath home
  journalContents <- readFile path
  Right journal <- runExceptT $ HL.parseJournalWith HL.journal True path journalContents

  let edit = editor "Edit" (str . concat) (Just 1) ""
      as = AppState edit DateQuestion journal (ctxList V.empty)

  void $ defaultMain app as

  where app = App { appDraw = draw
                  , appChooseCursor = showFirstCursor
                  , appHandleEvent = event
                  , appAttrMap = const def
                  , appLiftVtyEvent = id
                  , appStartEvent = return
                  } :: App AppState Event

expand = padBottom Max

ctxList v = (if V.null v then id else listMoveTo 0) $ list "Context" v 1
