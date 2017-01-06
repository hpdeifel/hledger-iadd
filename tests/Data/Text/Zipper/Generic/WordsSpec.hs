{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Text.Zipper.Generic.WordsSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Char

import           Data.Text.Zipper
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Zipper.Generic.Words

spec :: Spec
spec = do
  moveWordLeftSpec
  moveWordRightSpec

moveWordLeftSpec :: Spec
moveWordLeftSpec = describe "moveWordLeft" $ do
  it "does nothing at the start of the text" $
    moveWordLeft (zipLoc ["foo bar"] (0, 0)) `isAt` (0, 0)

  it "moves from middle of the word to the start" $
    moveWordLeft (zipLoc ["foo barfoo"] (0, 7)) `isAt` (0, 4)

  it "moves from end to beginning" $
    moveWordLeft (zipLoc ["barfoo"] (0, 6)) `isAt` (0, 0)

  it "stops at beginning of line if word boundary" $
    moveWordLeft (zipLoc ["foo", "bar"] (1, 2)) `isAt` (1, 0)

  it "moves across lines from beginning of line" $
    moveWordLeft (zipLoc ["foo", "bar"] (1, 0)) `isAt` (0, 0)

  it "skips multiple space characters" $
    moveWordLeft (zipLoc ["foo   bar"] (0, 6)) `isAt` (0, 0)

  it "skips multiple space characters across lines" $
    moveWordLeft (zipLoc ["foo  ", " bar"] (1, 1)) `isAt` (0, 0)

  it "always lands on the start of a word" $ property $ \(textlist :: [Text]) cursor ->
    isAtWordStart (moveWordLeft (zipLoc textlist cursor))

moveWordRightSpec :: Spec
moveWordRightSpec = describe "moveWordRight" $ do
  it "does nothing at the end of the text" $
    moveWordRight (zipLoc ["foo bar"] (0, 7)) `isAt` (0, 7)

  it "moves from middle of the word to its end" $
    moveWordRight (zipLoc ["barfoo foo"] (0, 2)) `isAt`(0, 6)

  it "moves from beginning to end" $
    moveWordRight (zipLoc ["barfoo"] (0, 0)) `isAt` (0, 6)

  it "stops at end of line if word boundary" $
    moveWordRight (zipLoc ["foo", "bar"] (0, 1)) `isAt` (0, 3)

  it "moves across lines from end of line" $
    moveWordRight (zipLoc ["foo", "bar"] (0, 3)) `isAt` (1, 3)

  it "skips multiple space characters" $
    moveWordRight (zipLoc ["foo    bar"] (0, 4)) `isAt` (0, 10)

  it "skips multiple space characters across lines" $
    moveWordRight (zipLoc ["foo   ", "  bar"] (0, 4)) `isAt` (1, 5)

  it "always lands at the end of a word" $ property $ \(textlist :: [Text]) cursor ->
    isAtWordEnd (moveWordRight (zipLoc textlist cursor))

-- Helpers

-- | Creates a zipper with initial content and cursor location
zipLoc :: [Text] -> (Int, Int) -> TextZipper Text
zipLoc content location = moveCursor location $ textZipper content Nothing

-- | Set the expectation that the given zipper is at the given cursor location
isAt :: TextZipper a -> (Int, Int) -> Expectation
isAt zipper loc = cursorPosition zipper `shouldBe` loc

isAtWordEnd :: TextZipper Text -> Property
isAtWordEnd zipper = counterexample (show zipper) $
  let
    (row, col) = cursorPosition zipper
    numLines = length (getText zipper)
    curLine = currentLine zipper
  in
    (col == T.length curLine && row == numLines)
    || ((col == T.length curLine || isSpace (T.index curLine col)) -- next is space
        && (col == 0 || not (isSpace (T.index curLine (col-1))))) -- previous is word

isAtWordStart :: TextZipper Text -> Property
isAtWordStart zipper = counterexample (show zipper) $
  let
    (row, col) = cursorPosition zipper
    curLine = currentLine zipper
  in
    (row == 0 && col == 0)
    || ((col == 0 || isSpace (T.index curLine (col-1))) -- previous is space
       && (col == T.length curLine || not (isSpace (T.index curLine col)))) -- next is word

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
