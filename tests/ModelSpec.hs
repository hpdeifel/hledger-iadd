{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TypeApplications #-}
module ModelSpec (spec) where

import           Test.Hspec

import           Control.Monad
import           Data.List
import           Data.Semigroup ((<>))

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
  describe "isDuplicateTransaction" isDuplicateTransactionSpec
  describe "isSubsetTransaction" isSubsetTransactionSpec

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
        suggest j german (AccountQuestion t "") `shouldReturn` Just (fst next)


  context "at the amount prompt" $ do

    it "suggests amounts from the similar transaction" $ do
      let postings = [("x", 1), ("y", 2), ("z", 3)]
          j = mkJournal [ ((2017, 1, 1), "Foo", postings) ]

      forM_ (zip (inits postings) postings) $ \(posts, next) -> do
        let t = mkTransaction ((2016, 1, 1), "Foo", posts)
        suggest j german (AmountQuestion (fst next) t "")
          `shouldReturn` Just ("€" <> T.pack (show $ snd next) <> ".00")

    it "suggests the balancing amount if accounts don't match with similar transaction" $ do
      let postings = [("x", 1), ("y", 2), ("z", 3)]
          j = mkJournal [ ((2017, 1, 1), "Foo", postings) ]
          t = mkTransaction ((2016, 1, 1), "Foo", [("foo", 3)])

      suggest j german (AmountQuestion "y" t "") `shouldReturn` Just "€-3.00"

    it "initially doesn't suggest an amount if there is no similar transaction" $ do
      let j = mkJournal [ ((2017, 1, 1), "Foo", [("x", 2), ("y", 3)]) ]
          t = mkTransaction ((2016, 1, 1), "Bar", [])

      suggest j german (AmountQuestion "y" t "") `shouldReturn` Nothing

    it "suggests the balancing amount if there is no similar transaction for the second account" $ do
      let j = mkJournal [ ((2017, 1, 1), "Foo", [("x", 2), ("y", 3)]) ]
          t = mkTransaction ((2016, 1, 1), "Bar", [("foo", 3)])

      suggest j german (AmountQuestion "y" t "") `shouldReturn` Just "€-3.00"


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

  it "includes accounts from the 'account directive'" $ do
    let j = (mkJournal [ ((2017, 1, 1), "Foo", [("x:y", 2)]) ]) { HL.jdeclaredaccounts = [("foo:bar", HL.nullaccountdeclarationinfo)]}
    accountsByFrequency j `shouldContain` ["foo:bar"]
    accountsByFrequency j `shouldContain` ["foo"]


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
    worksOn (FinalQuestion HL.nulltransaction False)

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
    worksOn (FinalQuestion HL.nulltransaction False)

  where
    worksOn :: Step -> Expectation
    worksOn step =
      let comment = "a fancy comment"
      in getTransactionComment (setTransactionComment comment step) `shouldBe` comment

isDuplicateTransactionSpec :: Spec
isDuplicateTransactionSpec = do
  it "considers exact copies as duplicates" $
    let trans = ((2017,9,23), "Test", [("Test", 1), ("Toast", -1)])
    in
      isDuplicateTransaction (mkJournal [trans]) (mkTransaction trans)

  it "ignores the order of postings" $
    let
      t1 = ((2017,9,23), "Test", [("Test", 1), ("Toast", -1)])
      t2 = ((2017,9,23), "Test", [("Toast", -1), ("Test", 1)])
    in
      isDuplicateTransaction (mkJournal [t1]) (mkTransaction t2)

  it "ignores comments and tags" $ do
    let
      t1 = mkTransaction ((2017,9,23), "Test", [("Test", 1), ("Toast", -1)])
      t2 = t1 { HL.tcomment = "Foo" }
      t3 = t1 { HL.ttags = [("Foo", "Bar")] }

    isDuplicateTransaction (HL.addTransaction t2 HL.nulljournal) t1 `shouldBe` True
    isDuplicateTransaction (HL.addTransaction t3 HL.nulljournal) t1 `shouldBe` True

  it "considers date and description" $ do
    let
      t1 = ((2017,9,23), "Test", [("Test", 1), ("Toast", -1)])
      t2 = ((2017,9,24), "Test", [("Test", 1), ("Toast", -1)])
      t3 = ((2017,9,23), "Foo", [("Test", 1), ("Toast", -1)])

    isDuplicateTransaction (mkJournal [t1]) (mkTransaction t2) `shouldBe` False
    isDuplicateTransaction (mkJournal [t1]) (mkTransaction t3) `shouldBe` False


  it "considers date, amount and account of postings" $ do
    let
      t1 = ((2017,9,23), "Test", [("Test", 1), ("Toast", -1)])
      t2 = ((2017,9,23), "Test", [("Foo", 1), ("Toast", -1)])
      t3 = ((2017,9,23), "Test", [("Test", 2), ("Toast", -1)])
      t4 = ((2017,9,23), "Test", [("Test", 1), ("Toast", -1), ("Foo", 2), ("Bar", -2)])

      t5p1 = (mkPosting ("Test", 1)) { HL.pdate = Just (fromGregorian 2017 9 23 )}
      t5 = (mkTransaction t1) { HL.tpostings = [t5p1, mkPosting ("Toast", -1)]}

    isDuplicateTransaction (mkJournal [t1]) (mkTransaction t2) `shouldBe` False
    isDuplicateTransaction (mkJournal [t1]) (mkTransaction t3) `shouldBe` False
    isDuplicateTransaction (mkJournal [t1]) (mkTransaction t4) `shouldBe` False
    isDuplicateTransaction (mkJournal [t1]) t5 `shouldBe` False

  it "ignores amount presentation" $ do
    let a1 = (HL.eur 0.5) { HL.astyle = HL.amountstyle}
        -- We use 'read' in the following because hledger-lib 1.19 changed the
        -- type of 'asprecision' from Int to 'AmountPrecision'. 'read' works in
        -- both cases.
        a2 = (HL.eur 0.5) { HL.astyle = HL.amountstyle { HL.asprecision = HL.Precision 15 } }

        p1 = mkPosting ("Test", -1)
        p2 = HL.nullposting { HL.paccount = "Toast", HL.pamount = HL.Mixed [a1] }
        p3 = HL.nullposting { HL.paccount = "Toast", HL.pamount = HL.Mixed [a2] }

        t0 = mkTransaction ((2017,9,23), "Test", [])
        t1 = t0 { HL.tpostings = [p1,p2,p2] }
        t2 = t0 { HL.tpostings = [p1,p3,p3] }

    isDuplicateTransaction (HL.addTransaction t1 HL.nulljournal) t2 `shouldBe` True

isSubsetTransactionSpec :: Spec
isSubsetTransactionSpec =
  it "ignores amount presentation" $ do
    let t1 = mkTransaction ((2021,3,12), "Test", [("Test", 1)])
        t2' = mkTransaction ((2021,3,12), "Test", [("Toast", -1)])
        testPosting = HL.nullposting
          { HL.paccount = "Test"
          , HL.pamount = HL.mixed [ (HL.eur 1) { HL.astyle = HL.amountstyle { HL.asdecimalpoint = Just ';' }}]}
        t2 = t2' { HL.tpostings =  testPosting : HL.tpostings t2' }
    isSubsetTransaction t1 t2 `shouldBe` True

-- Helpers

type Date = (Integer,Int,Int) -- y, d, m

-- | Creates a mock-journal from a list of transactions
--
-- Transactions consists of the date, a description and a list of postings in
-- for form of (account, amount)
mkJournal :: [(Date, Text, [(Text, Int)])] -> HL.Journal
mkJournal =
  foldl (\j t -> HL.addTransaction (mkTransaction t) j) HL.nulljournal

mkTransaction :: (Date, Text, [(Text, Int)]) -> HL.Transaction
mkTransaction ((year,month,day), desc, postings) = HL.nulltransaction
  { HL.tdate = fromGregorian year month day
  , HL.tdescription = desc
  , HL.tpostings = map mkPosting postings
  }

mkPosting :: (Text, Int) -> HL.Posting
mkPosting (account, amount) = HL.nullposting
  { HL.paccount = account
  , HL.pamount = HL.mixed [HL.eur (fromIntegral amount)]
  }
