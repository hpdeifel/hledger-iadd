-- | TODO Use the built-in wrapping feature in brick-0.20
module Brick.Widgets.WrappedText (wrappedText) where

import           Brick
import           Data.Text (Text)
import qualified Data.Text as T
import           Lens.Micro

-- | Widget like 'txt', but wrap all lines to fit on the screen.
--
-- Doesn't do word wrap, just breaks the line whenever the maximum width is
-- exceeded.
wrappedText :: Text -> Widget n
wrappedText theText = Widget Fixed Fixed $ do
  ctx <- getContext
  let newText = wrapLines (ctx^.availWidthL) theText
  render $ txt newText

-- | Wrap all lines in input to fit into maximum width.
--
-- Doesn't do word wrap, just breaks the line whenever the maximum width is
-- exceeded.
wrapLines :: Int -> Text -> Text
wrapLines width = T.unlines . concat . map wrap . T.lines
  where
    wrap = T.chunksOf width
