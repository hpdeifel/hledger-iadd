-- | Implements word movements for "Data.Text.Zipper"
--
-- Since these rely on character classes, the 'TZ.GenericTextZipper' constraint
-- is required.
module Data.Text.Zipper.Generic.Words
  ( moveWordLeft
  , moveWordRight
  ) where

import           Data.Char

import           Data.Text.Zipper
import qualified Data.Text.Zipper.Generic as TZ

-- | Move one word to the left
--
-- A word is defined as a consecutive string not satisfying isSpace. This
-- function always leaves the cursor at the beginning of a word (except at the
-- very start of the text).
moveWordLeft :: TZ.GenericTextZipper a => TextZipper a -> TextZipper a
moveWordLeft = moveWordLeft' False

moveWordLeft' :: TZ.GenericTextZipper a => Bool -> TextZipper a -> TextZipper a
moveWordLeft' inWord zipper = case charToTheLeft zipper of
  Nothing -> zipper  -- start of text
  Just c
    | isSpace c && not inWord
      -> moveWordLeft' False (moveLeft zipper)
    | not (isSpace c) && not inWord
      -> moveWordLeft' True zipper -- switch to skipping letters
    | not (isSpace c) && inWord
      -> moveWordLeft' True (moveLeft zipper)
    | otherwise
      -> zipper -- Done

-- | Move one word to the right
--
-- A word is defined as a consecutive string not satisfying isSpace. This
-- function always leaves the cursor at the end of a word (except at the very
-- end of the text).
moveWordRight :: TZ.GenericTextZipper a => TextZipper a -> TextZipper a
moveWordRight = moveWordRight' False

moveWordRight' :: TZ.GenericTextZipper a => Bool -> TextZipper a -> TextZipper a
moveWordRight' inWord zipper = case charToTheRight zipper of
  Nothing -> zipper -- end of text
  Just c
    | isSpace c && not inWord
      -> moveWordRight' False (moveRight zipper)
    | not (isSpace c) && not inWord
      -> moveWordRight' True zipper -- switch to skipping letters
    | not (isSpace c) && inWord
      -> moveWordRight' True (moveRight zipper)
    | otherwise
      -> zipper -- Done


-- Helpers

charToTheLeft :: TZ.GenericTextZipper a => TextZipper a -> Maybe Char
charToTheLeft zipper = case cursorPosition zipper of
  (0, 0) -> Nothing  -- Very start of text, no char left
  (_, 0) -> Just '\n' -- Start of line, simulate newline
  (_, x) -> Just (TZ.toList (currentLine zipper) !! (x-1))

charToTheRight :: TZ.GenericTextZipper a => TextZipper a -> Maybe Char
charToTheRight zipper
  | null (getText zipper) = Nothing
  | otherwise =
    let
      (row, col) = cursorPosition zipper
      content = getText zipper
      curLine = content !! row
      numLines = length content
    in
      if row == numLines - 1 && col == (TZ.length curLine) then
        Nothing -- very end
      else if col == (TZ.length curLine) then
        Just '\n' -- simulate newline
      else
        Just (TZ.toList curLine !! col)
