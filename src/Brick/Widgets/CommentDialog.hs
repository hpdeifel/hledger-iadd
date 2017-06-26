{-# LANGUAGE OverloadedStrings #-}

module Brick.Widgets.CommentDialog
  ( CommentWidget
  , commentWidget
  , renderCommentWidget
  , commentDialogComment
  , CommentAction(..)
  , handleCommentEvent
  ) where

import           Data.Monoid

import           Brick
import           Brick.Widgets.Edit hiding (handleEditorEvent)
import           Brick.Widgets.Dialog
import           Brick.Widgets.Center
import           Data.Text.Zipper
import           Graphics.Vty.Input
import qualified Data.Text as T
import           Data.Text (Text)

import           Brick.Widgets.Edit.EmacsBindings

data CommentWidget n = CommentWidget
  { origComment :: Text
  , textArea :: Editor Text n
  , dialogWidget :: Dialog ()
  , promptPrefix :: Text
  }

commentWidget :: n -> Text -> Text -> CommentWidget n
commentWidget name prompt comment =
  let
    title = "ESC: cancel, RET: accept, Alt-RET: New line"
    maxWidth = 80
    diag = dialog (Just title) Nothing maxWidth
    edit = editorText name Nothing comment
  in
    CommentWidget
      { origComment = comment
      , textArea = applyEdit gotoEnd edit
      , dialogWidget = diag
      , promptPrefix = prompt
      }

data CommentAction n = CommentContinue (CommentWidget n)
                     | CommentFinished Text

handleCommentEvent :: Event -> CommentWidget n -> EventM n (CommentAction n)
handleCommentEvent ev widget = case ev of
  EvKey KEsc [] -> return $ CommentFinished (origComment widget)
  EvKey KEnter [] -> return $ CommentFinished (commentDialogComment widget)
  EvKey KEnter [MMeta] -> return $ CommentContinue $
    widget { textArea = applyEdit breakLine (textArea widget) }
  _ -> do
    textArea' <- handleEditorEvent ev (textArea widget)
    return $ CommentContinue $
      CommentWidget (origComment widget) textArea' (dialogWidget widget) (promptPrefix widget)

renderCommentWidget :: (Ord n, Show n) => CommentWidget n -> Widget n
renderCommentWidget widget =
  let
    height = min (length (getEditContents (textArea widget)) + 4) 24
    drawer = txt . T.unlines
    textArea' =  padTop (Pad 1) $
      txt (promptPrefix widget <> ": ") <+> renderEditor drawer True (textArea widget)
  in
    vCenterLayer $ vLimit height $ renderDialog (dialogWidget widget) textArea'

commentDialogComment :: CommentWidget n -> Text
commentDialogComment = T.intercalate "\n" . getEditContents . textArea

gotoEnd :: Monoid a => TextZipper a -> TextZipper a
gotoEnd zipper =
  let
    lengths = lineLengths zipper
    (row, col) = (length lengths, last lengths)
  in
    moveCursor (row-1, col) zipper
