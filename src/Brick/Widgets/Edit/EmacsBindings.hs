{-# LANGUAGE TemplateHaskell, CPP #-}

-- | Widget like "Brick.Widgets.Edit", but with more emacs style keybindings.
--
-- This is also a complete wrapper around the "Brick.Widgets.Edit" API to retain
-- compatability with older brick versions.
--
-- See 'handleEditorEvent' for a list of added keybindings.
module Brick.Widgets.Edit.EmacsBindings
  ( Editor
  , editorText
  , getEditContents
  , applyEditM
  , applyEdit
  , editContentsL
  , handleEditorEvent
  , renderEditor
  ) where

import           Brick
import           Graphics.Vty
import qualified Brick.Widgets.Edit as E
import           Data.Text.Zipper
import           Data.Text (Text)
import           Lens.Micro.TH
import           Lens.Micro
import           Lens.Micro.Mtl

import           Data.Text.Zipper.Generic.Words

-- | Wrapper around 'E.Editor', but specialized to 'Text'
data Editor n = Editor {
  _origEditor :: E.Editor Text n,
  _drawingFunction :: [Text] -> Widget n
}

makeLenses ''Editor

-- | Wrapper for 'E.editorText' specialized to 'Text'
editorText :: n -> ([Text] -> Widget n)-> Maybe Int -> Text -> Editor n
editorText name draw linum content =
#if MIN_VERSION_brick(0,19,0)
  Editor (E.editorText name linum content) draw
#else
  Editor (E.editorText name draw linum content) draw
#endif

-- | Wrapper for 'E.getEditContents' specialized to 'Text'
getEditContents :: Editor n -> [Text]
getEditContents edit = edit ^. origEditor . to E.getEditContents

-- | Wrapper for 'E.applyEdit' specialized to 'Text'
applyEdit :: (TextZipper Text -> TextZipper Text) -> Editor n -> Editor n
applyEdit f edit = edit & origEditor %~ E.applyEdit f

applyEditM :: (TextZipper Text -> TextZipper Text) -> EventM n (Editor n) ()
applyEditM f = origEditor %= E.applyEdit f

-- | Wrapper for 'E.editContentsL' specialized to 'Text'
editContentsL :: Lens (Editor n) (Editor n) (TextZipper Text) (TextZipper Text)
editContentsL = origEditor . E.editContentsL

-- | Same as 'E.handleEditorEvent', but with more emacs-style keybindings and
-- specialized to 'Text'
--
-- Specifically:
--
--  - Ctrl-f: Move forward one character
--  - Ctrl-b: Move backward one character
--  - Alt-f: Move forward one word
--  - Alt-b: Move backward one word
--  - Alt-Backspace: Delete the previous word
--  - Ctrl-w: Delete the previous word
--  - Alt-d: Delete the next word
handleEditorEvent :: Eq n => Event -> EventM n (Editor n) ()
handleEditorEvent event = case event of
  EvKey (KChar 'f') [MCtrl] -> applyEditM moveRight
  EvKey (KChar 'b') [MCtrl] -> applyEditM moveLeft

  EvKey (KChar 'f') [MMeta] -> applyEditM moveWordRight
  EvKey (KChar 'b') [MMeta] -> applyEditM moveWordLeft

  EvKey KBS         [MMeta] -> applyEditM deletePrevWord
  EvKey (KChar 'w') [MCtrl] -> applyEditM deletePrevWord
  EvKey (KChar 'd') [MMeta] -> applyEditM deleteWord

  EvKey KHome       []      -> applyEditM gotoBOL
  EvKey KEnd        []      -> applyEditM gotoEOL

  _ -> zoom origEditor $ E.handleEditorEvent (VtyEvent event)


-- | Wrapper for 'E.renderEditor' specialized to 'Text'
renderEditor :: (Ord n, Show n) => Bool -> Editor n -> Widget n
renderEditor focus edit =
#if MIN_VERSION_brick(0,19,0)
  E.renderEditor (edit^.drawingFunction) focus (edit^.origEditor)
#else
  E.renderEditor focus (edit^.origEditor)
#endif
