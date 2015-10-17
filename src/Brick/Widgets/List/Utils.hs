module Brick.Widgets.List.Utils where

import Brick.Widgets.List
import Control.Lens
import qualified Data.Vector as V
import Data.Maybe

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
