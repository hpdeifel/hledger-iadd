{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Brick.Widgets.CommentDialog
  ( CommentWidget
  , commentWidget
  , renderCommentWidget
  , commentDialogComment
  , CommentAction(..)
  , handleCommentEvent
  ) where

import           Data.Semigroup ((<>))

import           Brick
import           Brick.Widgets.Dialog
import           Brick.Widgets.Center
import           Data.Text.Zipper
import           Graphics.Vty.Input
import qualified Data.Text as T
import           Data.Text (Text)

import           Lens.Micro
import           Lens.Micro.TH
import           Lens.Micro.Mtl

import           Brick.Widgets.Edit.EmacsBindings
import Control.Monad.Trans.Reader

data CommentWidget n = CommentWidget
  { _origComment :: Text
  , _textArea :: Editor n
  , _dialogWidget :: Dialog () n
  , _promptPrefix :: Text
  }

makeLenses ''CommentWidget

commentWidget :: Eq n => n -> Text -> Text -> CommentWidget n
commentWidget name prompt comment =
  let
    title = txt "ESC: cancel, RET: accept, Alt-RET: New line"
    maxWidth = 80
    diag = dialog (Just title) Nothing maxWidth
    edit = editorText name (txt . T.unlines) Nothing comment
  in
    CommentWidget
      { _origComment = comment
      , _textArea = applyEdit gotoEnd edit
      , _dialogWidget = diag
      , _promptPrefix = prompt
      }

data CommentAction = CommentContinue | CommentFinished Text

handleCommentEvent :: Eq n => Event -> EventM n (CommentWidget n) CommentAction
handleCommentEvent ev = case ev of
  EvKey KEsc [] -> CommentFinished <$> use origComment
  EvKey KEnter [] -> CommentFinished <$> gets commentDialogComment
  EvKey KEnter [MMeta] -> do
    zoom textArea $ applyEditM breakLine
    return CommentContinue
  _ -> do
    zoom textArea $ handleEditorEvent ev
    return CommentContinue

renderCommentWidget :: (Ord n, Show n) => CommentWidget n -> Widget n
renderCommentWidget widget =
  let
    height = min (length (getEditContents (widget^.textArea)) + 4) 24
    textArea' =  padTop (Pad 1) $
      txt (widget^.promptPrefix <> ": ") <+> renderEditor True (widget^.textArea)
  in
    vCenterLayer $ vLimit height $ renderDialog (widget^.dialogWidget) textArea'

commentDialogComment :: CommentWidget n -> Text
commentDialogComment = T.intercalate "\n" . getEditContents . _textArea

gotoEnd :: Monoid a => TextZipper a -> TextZipper a
gotoEnd zipper =
  let
    lengths = lineLengths zipper
    (row, col) = (length lengths, last lengths)
  in
    moveCursor (row-1, col) zipper
