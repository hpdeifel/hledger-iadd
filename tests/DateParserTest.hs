{-# LANGUAGE OverloadedStrings #-}

module DateParserTest (tests) where

import           Test.Tasty.HUnit
import           Test.Tasty

import           Data.Either
import           Data.Text (Text)
import qualified Data.Text as T

import           DateParser

tests :: TestTree
tests = testGroup "date parser" [ dateFormatTests
                                , dateTests
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
