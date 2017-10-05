{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UI.Theme
  ( Colorscheme(Colorscheme)
  , selectedBG
  , selectedFG
  , defaultColorscheme
  , buildAttrMap
  ) where

import           Control.Applicative ((<**>))
import           Data.Monoid ((<>))

import           Brick
import           Brick.Widgets.List (listSelectedAttr)
import           Data.Aeson
import qualified Data.Text as T
import           Graphics.Vty.Attributes
import           Lens.Micro
import           Lens.Micro.TH

import           Brick.Widgets.HelpMessage (helpAttr)

data Colorscheme = Colorscheme
  { _selectedBG :: Color
  , _selectedFG :: Color
  } deriving (Show,Eq)

makeLenses ''Colorscheme

defaultColorscheme :: Colorscheme
defaultColorscheme = Colorscheme
  { _selectedBG = white
  , _selectedFG = black
  }

instance FromJSON Color where
  parseJSON = withText "Color" $ \case
    t | t == "black" -> return black
      | t == "red" -> return red
      | t == "green" -> return green
      | t == "yellow" -> return yellow
      | t == "blue" -> return blue
      | t == "magenta" -> return magenta
      | t == "cyan" -> return cyan
      | t == "white" -> return white
      | otherwise -> fail $ "failed to parse color " <> T.unpack t

instance FromJSON Colorscheme where
  parseJSON = withObject "Colorscheme" $ \c -> pure defaultColorscheme
    <**> (c .:? "selected-bg" <&> optionally selectedBG)
    <**> (c .:? "selected-fg" <&> optionally selectedFG)

buildAttrMap :: Colorscheme -> AttrMap
buildAttrMap scheme = attrMap defAttr
  [ (listSelectedAttr, (scheme^.selectedFG) `on` (scheme^.selectedBG))
  , (helpAttr <> "title", fg green)
  ]

optionally :: ASetter s t b b -> Maybe b -> s -> t
optionally setter Nothing = over setter id
optionally setter (Just x) = set setter x
