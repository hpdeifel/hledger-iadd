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
  describe "accountsByFrequency" accByFreqSpec
  describe "setCurrentComment" setCurrentCommentSpec
  describe "setTransactionComment" setTransactionCommentSpec


suggestSpec :: Spec
suggestSpec = do
  context "at the account prompt" $ do

    it "suggests nothing for an empty journal" $
      suggest HL.nulljournal german (AccountQuestion HL.nulltransaction "")
        `shouldReturn` Nothing

    it "suggests the accounts in order" $ do
      let postings = [("x", 1), ("y", 2), ("z", 3)]
          j = mkJournal [ ((2017, 1, 1), "Foo", postings) ]

      forM_ (zip (inits postings) postings) $ \(posts, next) -> do
        let t = mkTransaction ((2016, 1, 1), "Foo", map (\(x,y) -> (x, y+1)) posts)
        suggest j german (AccountQuestion t "") `shouldReturn` (Just (fst next))


  context "at the amount prompt" $ do

    it "suggests amounts from the similar transaction" $ do
      let postings = [("x", 1), ("y", 2), ("z", 3)]
          j = mkJournal [ ((2017, 1, 1), "Foo", postings) ]

      forM_ (zip (inits postings) postings) $ \(posts, next) -> do
        let t = mkTransaction ((2016, 1, 1), "Foo", posts)
        suggest j german (AmountQuestion (fst next) t "")
          `shouldReturn` (Just ("€" <> (T.pack $ show $ snd next) <> ".00"))

    it "suggests the balancing amount if accounts don't match with similar transaction" $ do
      let postings = [("x", 1), ("y", 2), ("z", 3)]
          j = mkJournal [ ((2017, 1, 1), "Foo", postings) ]
          t = mkTransaction ((2016, 1, 1), "Foo", [("foo", 3)])

      suggest j german (AmountQuestion "y" t "") `shouldReturn` (Just "€-3.00")

    it "initially doesn't suggest an amount if there is no similar transaction" $ do
      let j = mkJournal [ ((2017, 1, 1), "Foo", [("x", 2), ("y", 3)]) ]
          t = mkTransaction ((2016, 1, 1), "Bar", [])

      suggest j german (AmountQuestion "y" t "") `shouldReturn` Nothing

    it "suggests the balancing amount if there is no similar transaction for the second account" $ do
      let j = mkJournal [ ((2017, 1, 1), "Foo", [("x", 2), ("y", 3)]) ]
          t = mkTransaction ((2016, 1, 1), "Bar", [("foo", 3)])

      suggest j german (AmountQuestion "y" t "") `shouldReturn` (Just "€-3.00")


accByFreqSpec :: Spec
accByFreqSpec = do
  it "sorts according to frequency" $ do
    let postings = [("x", 1), ("y", 2), ("y", 3)]
        j = mkJournal [ ((2017, 1, 1), "Foo", postings) ]

    accountsByFrequency j `shouldBe` ["y", "x"]

  it "includes subaccounts" $ do
    let j = mkJournal [ ((2017, 1, 1), "Foo", [("x:y", 2)]) ]
    accountsByFrequency j `shouldContain` ["x"]

  it "only counts explicit occurences for sorting" $ do
    let j = mkJournal [ ((2017, 1, 1), "Foo", [("x:y", 2), ("x:y", 3), ("x:z", 4)]) ]
    accountsByFrequency j `shouldBe` ["x:y", "x:z", "x"]


setCurrentCommentSpec :: Spec
setCurrentCommentSpec = do
  it "works at the date prompt" $
    worksOn (DateQuestion "")

  it "works at the description prompt" $
    worksOn (DescriptionQuestion (fromGregorian 2017 2 3) "")

  it "works at the account prompt" $
    worksOn (AccountQuestion HL.nulltransaction "")

  it "works at the amount prompt" $
    worksOn (AmountQuestion "foo" HL.nulltransaction "")

  it "works at the final prompt" $
    worksOn (FinalQuestion HL.nulltransaction)

  where
    worksOn :: Step -> Expectation
    worksOn step =
      let comment = "a fancy comment"
      in getCurrentComment (setCurrentComment comment step) `shouldBe` comment

setTransactionCommentSpec :: Spec
setTransactionCommentSpec = do
  it "works at the date prompt" $
    worksOn (DateQuestion "")

  it "works at the description prompt" $
    worksOn (DescriptionQuestion (fromGregorian 2017 2 3) "")

  it "works at the account prompt" $
    worksOn (AccountQuestion HL.nulltransaction "")

  it "works at the amount prompt" $
    worksOn (AmountQuestion "foo" HL.nulltransaction "")

  it "works at the final prompt" $
    worksOn (FinalQuestion HL.nulltransaction)

  where
    worksOn :: Step -> Expectation
    worksOn step =
      let comment = "a fancy comment"
      in getTransactionComment (setTransactionComment comment step) `shouldBe` comment

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
mkTransaction ((year,month,day), desc, postings) = HL.nulltransaction
  { HL.tdate = fromGregorian year month day
  , HL.tdescription = desc
  , HL.tpostings = map mkPosting postings
  }

  where
    mkPosting :: (Text, Int) -> HL.Posting
    mkPosting (account, amount) = HL.nullposting
      { HL.paccount = account
      , HL.pamount = HL.mixed [HL.eur (fromIntegral amount)]
      }

