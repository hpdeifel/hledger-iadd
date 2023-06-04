{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ConstraintKinds #-}
module Brick.Widgets.HelpMessage
       ( HelpWidget
       , Title
       , KeyBindings(..)
       , helpWidget
       , renderHelpWidget
       , helpAttr
       , resetHelpWidget
       , handleHelpEvent
       ) where

import Brick
import Brick.Widgets.Border
import Graphics.Vty
import Data.Text (Text)
import Data.List
import Lens.Micro

type Title = Text

-- [(Title, [(Key, Description)])]
newtype KeyBindings = KeyBindings [(Title, [(Text, Text)])]

data HelpWidget n = HelpWidget
  { keyBindings :: KeyBindings
  , name :: n
  }

type Name n = (Ord n, Show n)

helpWidget :: n -> KeyBindings -> HelpWidget n
helpWidget = flip HelpWidget

renderHelpWidget :: Name n => HelpWidget n -> Widget n
renderHelpWidget HelpWidget{keyBindings, name} =
  center $ renderHelpWidget' name keyBindings

center :: Widget n -> Widget n
center w = Widget Fixed Fixed $ do
  c <- getContext
  res <- render w
  let rWidth = res^.imageL.to imageWidth
      rHeight = res^.imageL.to imageHeight
      x = (c^.availWidthL `div` 2) - (rWidth `div` 2)
      y = (c^.availHeightL `div` 2) - (rHeight `div` 2)

  render $ translateBy (Location (x,y)) $ raw (res^.imageL)

renderHelpWidget' :: Name n => n -> KeyBindings -> Widget n
renderHelpWidget' name (KeyBindings bindings) = Widget Fixed Fixed $ do
  c <- getContext

  render $
    hLimit (min 80 $ c^.availWidthL) $
    vLimit (min 30 $ c^.availHeightL) $
    borderWithLabel (txt "Help") $
    viewport name Vertical $
    vBox $ intersperse (txt " ") $
    map (uncurry section) bindings

scroller :: HelpWidget n -> ViewportScroll n
scroller HelpWidget{name} = viewportScroll name

handleHelpEvent :: Event -> EventM n (HelpWidget n) ()
handleHelpEvent (EvKey k _) = case k of
  KChar 'j' -> gets scroller >>= \s -> vScrollBy s 1
  KDown     -> gets scroller >>= \s -> vScrollBy s 1
  KChar 'k' -> gets scroller >>= \s -> vScrollBy s (-1)
  KUp       -> gets scroller >>= \s -> vScrollBy s (-1)
  KChar 'g' -> gets scroller >>= \s -> vScrollToBeginning s
  KHome     -> gets scroller >>= \s -> vScrollToBeginning s
  KChar 'G' -> gets scroller >>= \s -> vScrollToEnd s
  KEnd      -> gets scroller >>= \s -> vScrollToEnd s
  KPageUp   -> gets scroller >>= \s -> vScrollPage s Up
  KPageDown -> gets scroller >>= \s -> vScrollPage s Down
  _         -> return ()
handleHelpEvent _ = return ()


resetHelpWidget :: HelpWidget n -> EventM n s ()
resetHelpWidget x = vScrollToBeginning (scroller x)

key :: Text -> Text -> Widget n
key k h =  withAttr (helpAttr <> attrName "key") (txt ("  " <> k))
       <+> padLeft Max (withAttr (helpAttr <> attrName "description") (txt h))

helpAttr :: AttrName
helpAttr = attrName "help"

section :: Title -> [(Text, Text)] -> Widget n
section title keys =  withAttr (helpAttr <> attrName "title") (txt (title <> ":"))
                  <=> vBox (map (uncurry key) keys)
