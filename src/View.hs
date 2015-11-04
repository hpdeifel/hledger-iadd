{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module View where

import           Brick
import           Brick.Widgets.List
import           Data.Monoid
import           Data.Text (Text)
import           Data.Time hiding (parseTime)
import qualified Hledger as HL

import           Model

viewState :: Step -> Widget
viewState DateQuestion = txt " "
viewState (DescriptionQuestion date) = str $
  formatTime defaultTimeLocale "%Y/%m/%d" date
viewState (AccountQuestion trans) = str $
  HL.showTransaction trans
viewState (AmountQuestion acc trans) = str $
  HL.showTransaction trans ++ "  " ++ acc
viewState (FinalQuestion trans) = str $
  HL.showTransaction trans

viewQuestion :: Step -> Widget
viewQuestion DateQuestion = txt "Date"
viewQuestion (DescriptionQuestion _) = txt "Description"
viewQuestion (AccountQuestion trans) = str $
  "Account " ++ show (numPostings trans + 1)
viewQuestion (AmountQuestion _ trans) = str $
  "Amount " ++ show (numPostings trans + 1)
viewQuestion (FinalQuestion trans) = txt $ mconcat $
  "Add this transaction to the journal? Y/n"
  : if HL.isTransactionBalanced Nothing trans then [] else ["\nTransaction not balanced!!!"]
viewContext :: List Text -> Widget
viewContext = flip renderList renderItem

viewSuggestion :: Maybe Text -> Widget
viewSuggestion Nothing = txt ""
viewSuggestion (Just t) = txt $ " (" <> t <> ")"

renderItem :: Bool -> Text -> Widget
renderItem True = withAttr listSelectedAttr . txt
renderItem False = txt

numPostings :: HL.Transaction -> Int
numPostings = length . HL.tpostings

