{-# LANGUAGE OverloadedStrings #-}

module DateParserTest (tests) where

import Test.Tasty.HUnit
import Test.Tasty

import DateParser

tests :: TestTree
tests = testGroup "date parser" [ dateFormatTests ]

dateFormatTests :: TestTree
dateFormatTests = testGroup "Date Spec"
  [ testCase "german" $
       parseDateFormat "%d[.[%m[.[%y]]]]" @?= Right german
  ]
