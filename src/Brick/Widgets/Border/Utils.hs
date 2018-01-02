-- | Extensions to "Brick.Widgets.Border"
module Brick.Widgets.Border.Utils (borderLeft) where

import Brick
import Brick.Widgets.Border
import Graphics.Vty

import Lens.Micro

-- | Draw a vertical border on the left side of a widget.
borderLeft :: Widget n -> Widget n
borderLeft wrapped = Widget (hSize wrapped) (vSize wrapped) $ do
  c <- getContext

  wrappedRes <- render $ hLimit (c^.availWidthL - 1)
                       $ wrapped

  let withBorder = vBorder <+> (Widget Fixed Fixed $ return wrappedRes)
      width = wrappedRes^.imageL.to imageWidth + 1
      height = wrappedRes^.imageL.to imageHeight

  render $ hLimit width $ vLimit height $ withBorder
