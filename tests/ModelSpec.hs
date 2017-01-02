{-# LANGUAGE OverloadedStrings #-}

module ModelSpec (spec) where

import           Test.Hspec

import           Control.Monad
import           Data.List
import           Data.Monoid

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar
import qualified Hledger as HL

import           DateParser
import           Model hiding (context)

spec :: Spec
spec = do
  describe "suggest" suggestSpec


suggestSpec :: Spec
suggestSpec = do
  context "at the account prompt" $ do

    it "suggests nothing for an empty journal" $
      suggest HL.nulljournal german (AccountQuestion HL.nulltransaction)
        `shouldReturn` Nothing

    it "suggests the accounts in order" $ do
      let postings = [("x", 1), ("y", 2), ("z", 3)]
          j = mkJournal [ ((2017, 1, 1), "Foo", postings) ]

      forM_ (zip (inits postings) postings) $ \(posts, next) -> do
        let t = mkTransaction ((2016, 1, 1), "Foo", map (\(x,y) -> (x, y+1)) posts)
        suggest j german (AccountQuestion t) `shouldReturn` (Just (fst next))


  context "at the amount prompt" $ do

    it "suggests amounts from the similar transaction" $ do
      let postings = [("x", 1), ("y", 2), ("z", 3)]
          j = mkJournal [ ((2017, 1, 1), "Foo", postings) ]

      forM_ (zip (inits postings) postings) $ \(posts, next) -> do
        let t = mkTransaction ((2016, 1, 1), "Foo", posts)
        suggest j german (AmountQuestion (fst next) t)
          `shouldReturn` (Just ("€" <> (T.pack $ show $ snd next) <> ".00"))

    it "suggests the balancing amount if accounts don't match with similar transaction" $ do
      let postings = [("x", 1), ("y", 2), ("z", 3)]
          j = mkJournal [ ((2017, 1, 1), "Foo", postings) ]
          t = mkTransaction ((2016, 1, 1), "Foo", [("foo", 3)])

      suggest j german (AmountQuestion "y" t) `shouldReturn` (Just "€-3.00")

-- Helpers

type Date = (Integer,Int,Int) -- y, d, m

-- | Creates a mock-journal from a list of transactions
--
-- Transactions consists of the date, a description and a list of postings in
-- for form of (account, amount)
mkJournal :: [(Date, Text, [(Text, Int)])] -> HL.Journal
mkJournal transactions =
  foldl (\j t -> HL.addTransaction (mkTransaction t) j) HL.nulljournal transactions

mkTransaction :: (Date, Text, [(Text, Int)]) -> HL.Transaction
mkTransaction ((year,month,day), desc, postings) = HL.Transaction
  { HL.tindex = 0
  , HL.tsourcepos = undefined
  , HL.tdate = fromGregorian year month day
  , HL.tdate2 = Nothing
  , HL.tstatus = HL.Uncleared
  , HL.tcode = ""
  , HL.tdescription = desc
  , HL.tcomment = ""
  , HL.ttags = []
  , HL.tpostings = map mkPosting postings
  , HL.tpreceding_comment_lines = ""
  }

  where
    mkPosting :: (Text, Int) -> HL.Posting
    mkPosting (account, amount) = HL.Posting
      { HL.pdate = Nothing
      , HL.pdate2 = Nothing
      , HL.pstatus = HL.Uncleared
      , HL.paccount = account
      , HL.pamount = HL.mixed [HL.eur (fromIntegral amount)]
      , HL.pcomment = ""
      , HL.ptype = HL.RegularPosting
      , HL.ptags = []
      , HL.pbalanceassertion = Nothing
      , HL.ptransaction = Nothing
      }
