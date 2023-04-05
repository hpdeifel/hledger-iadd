{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

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
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Hledger as HL

-- hledger-lib 1.17 will switch showTransaction to ISO date format, which means
-- that ISO dates yyyy-mm-dd will be added to the journal instead of yyyy/mm/dd.
--
-- Thus, for hledger-lib >=1.17, we also show the ISO format in the UI
#if !MIN_VERSION_hledger_lib(1,16,99)
import           Data.Time hiding (parseTime)
#endif

import           Model

viewState :: Step -> Widget n
viewState (DateQuestion comment) = txt $
  if T.null comment then " " else viewComment comment
viewState (DescriptionQuestion date comment) = txt $
#if MIN_VERSION_hledger_lib(1,16,99)
  T.pack (show date)
#else
  T.pack (formatTime defaultTimeLocale "%Y/%m/%d" date)
#endif
  <> viewComment comment
viewState (AccountQuestion trans comment) = txt $
  showTransaction trans <> viewComment comment
viewState (AmountQuestion acc trans comment) = txt $
  showTransaction trans <> "\n    " <> acc <> viewComment comment
viewState (FinalQuestion trans _) = txt $
  showTransaction trans

viewQuestion :: Step -> Widget n
viewQuestion (DateQuestion _) = txt "Date"
viewQuestion (DescriptionQuestion _ _) = txt "Description"
viewQuestion (AccountQuestion trans _) = str $
  "Account " ++ show (numPostings trans + 1)
viewQuestion (AmountQuestion _ trans _) = str $
  "Amount " ++ show (numPostings trans + 1)
viewQuestion (FinalQuestion _ duplicate) = txt $
  "Add this transaction to the journal?"
  <> (if duplicate then " (warning: duplicate)" else "") -- TODO Add better UI for duplicates
  <> " Y/n"

viewContext :: (Ord n, Show n) => List n Text -> Widget n
viewContext = renderList renderItem True

viewSuggestion :: Maybe Text -> Widget n
viewSuggestion Nothing = txt ""
viewSuggestion (Just t) = txt $ " (" <> t <> ")"

renderItem :: Bool -> Text -> Widget n
renderItem True = withAttr listSelectedAttr . padRight Max . txt
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


showTransaction :: HL.Transaction -> Text
showTransaction = T.stripEnd . HL.showTransaction
