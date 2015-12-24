{-# LANGUAGE OverloadedStrings #-}
module Brick.Widgets.HelpMessage
       ( Title
       , KeyBindings(..)
       , helpWidget
       , helpAttr
       , resetHelpWidget
       ) where

import Brick
import Brick.Markup
import Brick.Widgets.Border
import Graphics.Vty
import Data.Text (Text)
import Data.Monoid
import Data.List
import Control.Lens

type Title = Text

-- [(Title, [(Key, Description)])]
newtype KeyBindings = KeyBindings [(Title, [(Text, Text)])]

helpWidget :: KeyBindings -> Widget
helpWidget bindings = center $ helpWidget' bindings

center :: Widget -> Widget
center w = Widget Fixed Fixed $ do
  c <- getContext
  res <- render w
  let rWidth = res^.imageL.to imageWidth
      rHeight = res^.imageL.to imageHeight
      x = (c^.availWidthL `div` 2) - (rWidth `div` 2)
      y = (c^.availHeightL `div` 2) - (rHeight `div` 2)

  render $ translateBy (Location (x,y)) $ raw (res^.imageL)

helpWidget' :: KeyBindings -> Widget
helpWidget' (KeyBindings bindings) = Widget Fixed Fixed $ do
  c <- getContext

  render $
    hLimit (min 80 $ c^.availWidthL) $
    vLimit (min 30 $ c^.availHeightL) $
    borderWithLabel (txt "Help") $
    viewport "helpViewport" Vertical $
    vBox $ intersperse (txt " ") $
    map (uncurry section) bindings

scroller :: ViewportScroll
scroller = viewportScroll "helpViewport"

instance HandleEvent KeyBindings where
  handleEvent (EvKey k _) b = case k of
    KChar 'j' -> vScrollBy scroller 1 >> return b
    KDown     -> vScrollBy scroller 1 >> return b
    KChar 'k' -> vScrollBy scroller (-1) >> return b
    KUp       -> vScrollBy scroller (-1) >> return b
    KChar 'g' -> vScrollToBeginning scroller >> return b
    KHome     -> vScrollToBeginning scroller >> return b
    KChar 'G' -> vScrollToEnd scroller >> return b
    KEnd      -> vScrollToEnd scroller >> return b
    KPageUp   -> vScrollPage scroller Up >> return b
    KPageDown -> vScrollPage scroller Down >> return b
    _         -> return b

  handleEvent _ b = return b


resetHelpWidget :: EventM ()
resetHelpWidget = vScrollToBeginning scroller

key :: Text -> Text -> Widget
key k h =  markup (("  " <> k) @? (helpAttr <> "key"))
       <+> padLeft Max (markup (h @? (helpAttr <> "description")))

helpAttr :: AttrName
helpAttr = "help"

section :: Title -> [(Text, Text)] -> Widget
section title keys =  markup ((title <> ":") @? (helpAttr <> "title"))
                  <=> vBox (map (uncurry key) keys)
