{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DateParserSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances()

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
  dateCompletionTests
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

dateCompletionTests :: Spec
dateCompletionTests = describe "date completion" $ do
  it "today" $
    parseGerman 2004 7 31 "31.7.2004" `shouldBe` Right (fromGregorian 2004 7 31)

  it "today is a leap day" $
    parseGerman 2012 2 29 "29.2.2012" `shouldBe` Right (fromGregorian 2012 2 29)

  it "skips to previous month" $
    parseGerman 2016 9 20 "21" `shouldBe` Right (fromGregorian 2016 08 21)

  it "stays in month if possible" $
    parseGerman 2016 8 30 "21" `shouldBe` Right (fromGregorian 2016 08 21)

  it "skips to previous month to reach the 31st" $
    parseGerman 2016 8 30 "31" `shouldBe` Right (fromGregorian 2016 07 31)

  it "skips to an earlier month to reach the 31st" $
    parseGerman 2016 7 30 "31" `shouldBe` Right (fromGregorian 2016 05 31)

  it "skips to the previous year if necessary" $
    parseGerman 2016 9 30 "2.12." `shouldBe` Right (fromGregorian 2015 12 2)

  it "skips to the previous years if after a leap year" $
    parseGerman 2017 3 10 "29.2" `shouldBe` Right (fromGregorian 2016 02 29)

  it "even might skip to a leap year 8 years ago" $
    parseGerman 2104 2 27 "29.2" `shouldBe` Right (fromGregorian 2096 02 29)

  it "some date in the near future" $
    parseGerman 2016 2 20 "30.11.2016" `shouldBe` Right (fromGregorian 2016 11 30)

  it "some date in the far future" $
    parseGerman 2016 2 20 "30.11.3348" `shouldBe` Right (fromGregorian 3348 11 30)

  it "last october" $
    (do
      monthOnly <- parseDateFormat "%m"
      parseDate (fromGregorian 2016 9 15) monthOnly "10"
    ) `shouldBe` Right (fromGregorian 2015 10 31)

  it "last november" $
    (do
      monthOnly <- parseDateFormat "%m"
      parseDate (fromGregorian 2016 9 15) monthOnly "11"
    ) `shouldBe` Right (fromGregorian 2015 11 30)

  it "next november" $
    (do
      yearMonth <- parseDateFormat "%y.%m"
      parseDate (fromGregorian 2016 9 15) yearMonth "2016.11"
    ) `shouldBe` Right (fromGregorian 2016 11 1)

  it "next january" $
    (do
      yearMonth <- parseDateFormat "%y.%m"
      parseDate (fromGregorian 2016 9 15) yearMonth "2017.1"
    ) `shouldBe` Right (fromGregorian 2017 1 1)

  it "last january" $
    (do
      yearMonth <- parseDateFormat "%y.%m"
      parseDate (fromGregorian 2016 9 15) yearMonth "2016.1"
    ) `shouldBe` Right (fromGregorian 2016 1 31)

  it "literally yesterday" $ do
    parseGerman 2018 10 18 "yesterday" `shouldBe` Right (fromGregorian 2018 10 17)
    parseGerman 2018 10 18 "yest" `shouldBe` Right (fromGregorian 2018 10 17)

  it "literally today" $ do
    parseGerman 2018 10 18 "today" `shouldBe` Right (fromGregorian 2018 10 18)

  it "literally tomorrow" $ do
    parseGerman 2018 10 18 "tomorrow" `shouldBe` Right (fromGregorian 2018 10 19)

  it "literally monday" $ do
    parseGerman 2018 10 18 "monday" `shouldBe` Right (fromGregorian 2018 10 15)
    parseGerman 2018 10 18 "mon" `shouldBe` Right (fromGregorian 2018 10 15)

  it "literally tuesday" $ do
    parseGerman 2018 10 18 "tuesday" `shouldBe` Right (fromGregorian 2018 10 16)
    parseGerman 2018 10 18 "tues" `shouldBe` Right (fromGregorian 2018 10 16)
    parseGerman 2018 10 18 "tue" `shouldBe` Right (fromGregorian 2018 10 16)

  it "literally wednesday" $ do
    parseGerman 2018 10 18 "wednesday" `shouldBe` Right (fromGregorian 2018 10 17)
    parseGerman 2018 10 18 "wed" `shouldBe` Right (fromGregorian 2018 10 17)

  it "literally thursday" $ do
    parseGerman 2018 10 18 "thursday" `shouldBe` Right (fromGregorian 2018 10 18)
    parseGerman 2018 10 18 "thur" `shouldBe` Right (fromGregorian 2018 10 18)

  it "literally friday" $ do
    parseGerman 2018 10 18 "friday" `shouldBe` Right (fromGregorian 2018 10 12)
    parseGerman 2018 10 18 "fri" `shouldBe` Right (fromGregorian 2018 10 12)

  it "literally saturday" $ do
    parseGerman 2018 10 18 "saturday" `shouldBe` Right (fromGregorian 2018 10 13)
    parseGerman 2018 10 18 "sat" `shouldBe` Right (fromGregorian 2018 10 13)

  it "literally sunday" $ do
    parseGerman 2018 10 18 "sunday" `shouldBe` Right (fromGregorian 2018 10 14)
    parseGerman 2018 10 18 "sun" `shouldBe` Right (fromGregorian 2018 10 14)

  it "literally satan" $ do
    parseGerman 2018 10 18 "satan" `shouldSatisfy` isLeft

  where
    parseGerman :: Integer -> Int -> Int -> String -> Either Text Day
    parseGerman y m d str = parseDate (fromGregorian y m d)  german (T.pack str)

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
