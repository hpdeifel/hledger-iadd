{-# LANGUAGE OverloadedStrings #-}

module Brick.Widgets.BetterDialog ( dialog )where

import Brick
import Brick.Markup
import Brick.Widgets.Border
import Graphics.Vty
import Data.Text (Text)
import Data.Monoid
import Data.List
import Control.Lens

dialog :: Text -> Text -> Widget
dialog title = center . dialog' title

-- TODO Remove duplication from HelpMessage
center :: Widget -> Widget
center w = Widget Fixed Fixed $ do
  c <- getContext
  res <- render w
  let rWidth = res^.imageL.to imageWidth
      rHeight = res^.imageL.to imageHeight
      x = (c^.availWidthL `div` 2) - (rWidth `div` 2)
      y = (c^.availHeightL `div` 2) - (rHeight `div` 2)

  render $ translateBy (Location (x,y)) $ raw (res^.imageL)

dialog' :: Text -> Text -> Widget
dialog' title content = Widget Fixed Fixed $ do
  c <- getContext

  render $
    hLimit (min 80 $ c^.availWidthL) $
    vLimit (min 30 $ c^.availHeightL) $
    borderWithLabel (txt title) $
          txt " "
      <=> (txt " " <+> txt content <+> txt " ")
      <=> txt " "
