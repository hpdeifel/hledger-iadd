{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Brick
import           Brick.Widgets.Edit
import           Brick.Widgets.Border

import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Zipper
import           Data.Time
import           Graphics.Vty.Input.Events
import qualified Hledger as HL

data AppState = AppState
  { asEditor :: Editor
  , asWizardState :: WizardState
  }

data WizardState = DateQuestion
                 | DescriptionQuestion Day

draw :: AppState -> [Widget]
draw as = [ui]
  where ui =  renderState (asWizardState as)
          <=> hBorder
          <=> (renderQuestion (asWizardState as) <+> renderEditor (asEditor as))
          <=> hBorder
          <=> str "Some conceptual information"

event :: AppState -> Event -> EventM (Next AppState)
event as ev = case ev of
  EvKey KEsc [] -> halt as
  EvKey KEnter [] -> continue (nextStep as)
  _ -> (AppState <$> handleEvent ev (asEditor as) <*> return (asWizardState as)) >>= continue

renderState :: WizardState -> Widget
renderState DateQuestion = txt " "
renderState (DescriptionQuestion date) = txt $ mconcat
  [ T.pack $ formatTime defaultTimeLocale "%d.%m.%Y" date
  ]

renderQuestion :: WizardState -> Widget
renderQuestion DateQuestion = txt "Date: "
renderQuestion (DescriptionQuestion _) = txt "Description: "

nextStep :: AppState -> AppState
nextStep as = case asWizardState as of
  DateQuestion -> as { asWizardState = DescriptionQuestion (T.pack $ concat $ getEditContents $ asEditor as)
                     , asEditor = asEditor as & editContentsL .~ stringZipper [""] (Just 1)
                     }
  (DescriptionQuestion _) -> as

parseTime :: Text -> Maybe Text
parseTime t = case T.splitOn "." t of
  [d] -> Just _
  [d,m] -> Just _
  [d,m,y] -> Just _
  _ -> Nothing

main :: IO ()
main =
  let edit = editor "Edit" (str . concat) (Just 1) "Foo"
      as = AppState edit DateQuestion
  in void $ defaultMain app as

  where app = App { appDraw = draw
                  , appChooseCursor = showFirstCursor
                  , appHandleEvent = event
                  , appAttrMap = const def
                  , appLiftVtyEvent = id
                  , appStartEvent = return
                  } :: App AppState Event
