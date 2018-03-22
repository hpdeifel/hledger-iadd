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
import Brick.Markup
import Brick.Widgets.Border
import Graphics.Vty
import Data.Text (Text)
import Data.Monoid ((<>))
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

handleHelpEvent :: HelpWidget n -> Event -> EventM n (HelpWidget n)
handleHelpEvent help (EvKey k _) = case k of
  KChar 'j' -> vScrollBy (scroller help) 1 >> return help
  KDown     -> vScrollBy (scroller help) 1 >> return help
  KChar 'k' -> vScrollBy (scroller help) (-1) >> return help
  KUp       -> vScrollBy (scroller help) (-1) >> return help
  KChar 'g' -> vScrollToBeginning (scroller help) >> return help
  KHome     -> vScrollToBeginning (scroller help) >> return help
  KChar 'G' -> vScrollToEnd (scroller help) >> return help
  KEnd      -> vScrollToEnd (scroller help) >> return help
  KPageUp   -> vScrollPage (scroller help) Up >> return help
  KPageDown -> vScrollPage (scroller help) Down >> return help
  _         -> return help
handleHelpEvent help _ = return help


resetHelpWidget :: HelpWidget n -> EventM n ()
resetHelpWidget = vScrollToBeginning . scroller

key :: Text -> Text -> Widget n
key k h =  markup (("  " <> k) @? (helpAttr <> "key"))
       <+> padLeft Max (markup (h @? (helpAttr <> "description")))

helpAttr :: AttrName
helpAttr = "help"

section :: Title -> [(Text, Text)] -> Widget n
section title keys =  markup ((title <> ":") @? (helpAttr <> "title"))
                  <=> vBox (map (uncurry key) keys)
