-- | Widget like "Brick.Widgets.Edit", but with more emacs style keybindings.
--
-- See 'handleEditorEvent' for a list of added keybindings.
module Brick.Widgets.Edit.EmacsBindings
  ( handleEditorEvent
  , module Brick.Widgets.Edit
  ) where

import           Brick
import           Graphics.Vty
import qualified Brick.Widgets.Edit as E
import           Brick.Widgets.Edit hiding (handleEditorEvent)
import           Data.Text.Zipper

-- | Same as 'E.handleEditorEvent', but with more emacs-style keybindings
--
-- Specifically:
--
--  - Ctrl-f: Move forward one character
--  - Ctrl-b: Move backward one character
handleEditorEvent :: (Eq t, Monoid t) => Event -> Editor t n -> EventM n (Editor t n)
handleEditorEvent event edit = case event of
  EvKey (KChar 'f') [MCtrl] -> return $ applyEdit moveRight edit
  EvKey (KChar 'b') [MCtrl] -> return $ applyEdit moveLeft edit
  _ -> E.handleEditorEvent event edit
