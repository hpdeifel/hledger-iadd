{-# LANGUAGE OverloadedStrings #-}

module DateParserTest (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Data.Either
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Calendar.WeekDate

import           DateParser

tests :: TestTree
tests = testGroup "date parser" [ dateFormatTests
                                , dateTests
                                , printTests
                                ]

dateFormatTests :: TestTree
dateFormatTests = testGroup "Date Format"
  [ testCase "german" $
       parseDateFormat "%d[.[%m[.[%y]]]]" @?= Right german
  ]

dateTests :: TestTree
dateTests = testGroup "Date Parser"
  [ testCase "non optional fields are actually requierd" $
      shouldFail "%d-%m-%y" "05"

  , testProperty "Week day actually returns the right week day" $
      weekDayProp

  , testProperty "Week day smaller than current date" $
      weekDaySmallerProp
  ]

printTests :: TestTree
printTests = testGroup "Date Printer"
  [ testProperty "Printing and reading are inverse" $
      printReadProp german

  , testCase "Padding with short years works" $ do
      withDateFormat ("%d-[%m-[%y]]") $ \format ->
        printDate format (fromGregorian 2015 2 1) @?= "01-02-15"

      withDateFormat ("%d-[%m-[%y]]") $ \format ->
        printDate format (fromGregorian 1999 2 1) @?= "01-02-1999"

  , testCase "Padding with long years works" $
      withDateFormat ("%d-[%m-[%Y]]") $ \format ->
        printDate format (fromGregorian 2015 2 1) @?= "01-02-2015"
  ]

withDateFormat :: Text -> (DateFormat -> Assertion) -> Assertion
withDateFormat date action = case parseDateFormat date of
  Left err -> assertFailure (show err)
  Right format -> action format

shouldFail :: Text -> Text -> Assertion
shouldFail format date = withDateFormat format $ \format' -> do
  res <- parseDateWithToday format' date
  assertBool ("Should fail but parses: " ++ (T.unpack format)
              ++ " / " ++ (T.unpack date) ++ " as " ++ show res)
    (isLeft res)

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
