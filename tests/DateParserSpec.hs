 {-# LANGUAGE OverloadedStrings #-}

module DateParserSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad
import           Data.Either
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Calendar.WeekDate

import           DateParser

spec :: Spec
spec = do
  dateFormatTests
  dateTests
  printTests

dateFormatTests :: Spec
dateFormatTests = describe "date format parser" $
  it "parses the german format correctly" $
    parseDateFormat "%d[.[%m[.[%y]]]]" `shouldBe` Right german

dateTests :: Spec
dateTests = describe "date parser" $ do
  it "actually requires non-optional fields" $
    shouldFail "%d-%m-%y" "05"

  describe "weekDay" $ do
    it "actually returns the right week day" $ property
      weekDayProp

    it "is always smaller than the current date" $ property
      weekDaySmallerProp

printTests :: Spec
printTests = describe "date printer" $ do
  it "is inverse to reading" $ property $
      printReadProp german

  it "handles short years correctly" $ do
      withDateFormat ("%d-[%m-[%y]]") $ \format ->
        printDate format (fromGregorian 2015 2 1) `shouldBe` "01-02-15"

      withDateFormat ("%d-[%m-[%y]]") $ \format ->
        printDate format (fromGregorian 1999 2 1) `shouldBe` "01-02-1999"

  it "handles long years correctly" $
      withDateFormat ("%d-[%m-[%Y]]") $ \format ->
        printDate format (fromGregorian 2015 2 1) `shouldBe` "01-02-2015"

withDateFormat :: Text -> (DateFormat -> Expectation) -> Expectation
withDateFormat date action = case parseDateFormat date of
  Left err -> expectationFailure (show err)
  Right format -> action format

shouldFail :: Text -> Text -> Expectation
shouldFail format date = withDateFormat format $ \format' -> do
  res <- parseDateWithToday format' date
  unless (isLeft res) $
    expectationFailure ("Should fail but parses: " ++ (T.unpack format)
                        ++ " / " ++ (T.unpack date) ++ " as " ++ show res)

weekDayProp :: Property
weekDayProp =
  forAll (ModifiedJulianDay <$> (arbitrary `suchThat` (>= 7))) $ \current ->
  forAll (choose (1, 7)) $ \wday ->
    wday === getWDay (weekDay wday current)

  where getWDay :: Day -> Int
        getWDay d = let (_, _, w) = toWeekDate d in w

weekDaySmallerProp :: Property
weekDaySmallerProp =
  forAll (ModifiedJulianDay <$> (arbitrary `suchThat` (>= 7))) $ \current ->
  forAll (choose (1, 7)) $ \wday ->
    current >= weekDay wday current

printReadProp :: DateFormat -> Day -> Property
printReadProp format day = case parseDate day format (printDate format day) of
  Left err -> counterexample (T.unpack err) False
  Right res -> res === day

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary
