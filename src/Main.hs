{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import           Brick
import           Brick.Widgets.Edit
import           Brick.Widgets.Border

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Zipper
import           Data.Time hiding (parseTime)
import           Graphics.Vty.Input.Events
import qualified Hledger as HL

data AppState = AppState
  { asEditor :: Editor
  , asWizardState :: WizardState
  }

data WizardState = DateQuestion
                 | DescriptionQuestion Day
                 | AccountQuestion Day Text [HL.Posting]

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
  EvKey KEnter [] -> liftIO (nextStep as) >>= continue
  _ -> (AppState <$> handleEvent ev (asEditor as) <*> return (asWizardState as)) >>= continue

renderState :: WizardState -> Widget
renderState DateQuestion = txt " "
renderState (DescriptionQuestion date) = txt $ mconcat
  [ "Date: " <> (T.pack $ formatTime defaultTimeLocale "%d.%m.%Y" date) ]
renderState (AccountQuestion date desc postings) = txt $ mconcat
  [ "Date: " <> (T.pack $ formatTime defaultTimeLocale "%d.%m.%Y" date)
  , "Description: " <> desc
  ]

renderQuestion :: WizardState -> Widget
renderQuestion DateQuestion = txt "Date: "
renderQuestion (DescriptionQuestion _) = txt "Description: "
renderQuestion (AccountQuestion _ _ ls) = txt $ "Account" <> (T.pack $ show $ length ls) <> ": "

nextStep :: AppState -> IO AppState
nextStep as = case asWizardState as of
  DateQuestion -> do
    parseTime (editText as) >>= \case
      Nothing -> return as -- TODO Show error
      Just day -> return $ as
        { asWizardState = DescriptionQuestion day
        , asEditor = asEditor as & editContentsL .~ stringZipper [""] (Just 1)
        }
  (DescriptionQuestion d) -> return as
    { asWizardState = AccountQuestion d (editText as) []
    , asEditor = asEditor as & editContentsL .~ stringZipper [""] (Just 1)
    }

editText = T.pack . concat . getEditContents . asEditor

parseTime :: Text -> IO (Maybe Day)
parseTime t = do
  (currentYear, currentMonth, currentDay) <- toGregorian . utctDay <$> getCurrentTime
  case filter (not . T.null) $ T.splitOn "." t of
    [d] -> return $ Just $ fromGregorian currentYear currentMonth (parseInt d)
    [d,m] -> return $ Just $ fromGregorian currentYear (parseInt m) (parseInt d)
    [d,m,y] -> return $ Just $ fromGregorian (parseInt y) (parseInt m) (parseInt d)
    _ -> return Nothing

-- TODO: Handle failure
parseInt :: (Read a, Num a) => Text -> a
parseInt t = read $ T.unpack t

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
