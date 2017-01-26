{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module View
  ( viewState
  , viewQuestion
  , viewContext
  , viewSuggestion
  , viewMessage
  ) where

import           Brick
import           Brick.Widgets.List
import           Brick.Widgets.WrappedText
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time hiding (parseTime)
import qualified Hledger as HL

import           Model

viewState :: Step -> Widget n
viewState (DateQuestion comment) = txt $
  if T.null comment then " " else viewComment comment
viewState (DescriptionQuestion date comment) = txt $
  T.pack (formatTime defaultTimeLocale "%Y/%m/%d" date)
  <> viewComment comment
viewState (AccountQuestion trans comment) = txt $
  T.pack (HL.showTransaction trans)
  <> viewComment comment
viewState (AmountQuestion acc trans comment) = txt $
  T.pack (HL.showTransaction trans) <> "  " <> acc
  <> viewComment comment
viewState (FinalQuestion trans) = str $
  HL.showTransaction trans

viewQuestion :: Step -> Widget n
viewQuestion (DateQuestion _) = txt "Date"
viewQuestion (DescriptionQuestion _ _) = txt "Description"
viewQuestion (AccountQuestion trans _) = str $
  "Account " ++ show (numPostings trans + 1)
viewQuestion (AmountQuestion _ trans _) = str $
  "Amount " ++ show (numPostings trans + 1)
viewQuestion (FinalQuestion _) = txt $
  "Add this transaction to the journal? Y/n"

viewContext :: (Ord n, Show n) => List n Text -> Widget n
viewContext = renderList renderItem True

viewSuggestion :: Maybe Text -> Widget n
viewSuggestion Nothing = txt ""
viewSuggestion (Just t) = txt $ " (" <> t <> ")"

renderItem :: Bool -> Text -> Widget n
renderItem True = withAttr listSelectedAttr . txt
renderItem False = txt

numPostings :: HL.Transaction -> Int
numPostings = length . HL.tpostings

-- TODO Adding " " to an empty message isn't required for vty >= 5.14
--      => Remove this, once 5.14 becomes lower bound
viewMessage :: Text -> Widget n
viewMessage msg = wrappedText (if T.null msg then " " else msg)

viewComment :: Text -> Text
viewComment comment
  | T.null comment = ""
  | otherwise      = T.unlines $ map ("  ; " <>) $ T.lines comment
