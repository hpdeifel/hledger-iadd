module Brick.Widgets.List.Utils where

import Brick
import Graphics.Vty
import Brick.Widgets.List
import Control.Lens
import qualified Data.Vector as V
import Data.Maybe

-- listAppend :: e -> List e -> List e
-- listAppend e l = listInsert (l^.listElementsL.to V.length) e l

-- listRemoveEq :: Eq e => e -> List e -> List e
-- listRemoveEq e l = case l^.listElementsL.to (V.elemIndex e) of
--   Nothing -> l
--   Just i  -> listRemove i l

-- listSwap :: Eq e => e -> e -> List e -> List e
-- listSwap old new l = case l^.listElementsL.to (V.elemIndex old) of
--   Nothing -> listAppend new l
--   Just i  -> listMoveTo i $ listInsert i new $ listRemove i l

-- handleHJKLEvent :: Event -> List e -> EventM (List e)
-- handleHJKLEvent ev lst = case ev of
--   EvKey (KChar 'j') [] -> return $ listMoveDown lst
--   EvKey (KChar 'k') [] -> return $ listMoveUp lst
--   EvKey (KChar 'g') [] -> return $ listMoveTo 0 lst
--   EvKey (KChar 'G') [] -> return $ listMoveTo (lst^.listElementsL.to V.length) lst
--   _                    -> handleEvent ev lst

-- | Replace the contents of a list with a new set of elements but preserve the
-- currently selected index.
--
-- This is a version of listReplace that doesn't try to be smart, but assumes
-- that all the elements in one list are distinct.
--
-- listReplace itself is broken as of brick-0.2 due to a bogus implementation of
-- the `merge` function.
listSimpleReplace :: Eq e => V.Vector e -> List e -> List e
listSimpleReplace elems oldList =
  let selected = flip V.elemIndex elems . snd =<< listSelectedElement oldList
      newSelected = if V.null elems
                       then Nothing
                       else Just $ fromMaybe 0 selected
  in oldList & listElementsL .~ elems & listSelectedL .~ newSelected
