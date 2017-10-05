{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config
  ( MatchAlgo(..)
  , Config(Config)
  , ledgerFile
  , dateFormat
  , matchAlgo
  , defaultConfig
  , parseConfigText
  , parseConfigFile
  , prettyPrintConfig
  , replaceLeadingTilde
  , replaceLeadingTildeByHome
  ) where

import           Control.Applicative ((<**>))
import           Data.Monoid ((<>))
import           Control.Exception (SomeException,try)

import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)
import           Lens.Micro.Platform
import           System.Directory (getHomeDirectory)
import           Text.Toml

data MatchAlgo = Fuzzy | Substrings
  deriving (Eq, Show)


instance FromJSON MatchAlgo where
  parseJSON = withText "Completion Engine" $ \t ->
    if t == "fuzzy" then pure Fuzzy
    else if t == "substrings" then pure Substrings
    else fail $ T.unpack $
         "Unknown completion engine \"" <> t <> "\": "
         <> " \"substrings\" or \"fuzzy\" expected"

data Config = Config
  { _ledgerFile :: FilePath
  , _dateFormat :: String
  , _matchAlgo :: MatchAlgo
  } deriving (Show, Eq)

makeLenses ''Config

parseConfigText :: FilePath -> Text -> Either Text Config
parseConfigText path txt = case parseTomlDoc path txt of
  Left err -> Left (T.pack $ show err)
  Right toml -> case fromJSON (toJSON toml) of
    Error err -> Left (T.pack err)
    Success res -> Right res

parseConfigFile :: FilePath -> IO (Either Text Config)
parseConfigFile path =
  try (T.readFile path) >>= \case
    Left (_ :: SomeException) -> return (Right defaultConfig)
    Right txt -> do
      let res = parseConfigText path txt
      traverseOf (_Right.ledgerFile) replaceLeadingTildeByHome res

defaultConfig :: Config
defaultConfig = Config
  { _ledgerFile = "~/.hledger.journal"
  , _dateFormat = "[[%y/]%m/]%d"
  , _matchAlgo = Substrings
  }

prettyPrintConfig :: Config -> Text
prettyPrintConfig c = T.unlines
 [ "# Path to the journal file"
 , "file = " <> c ^. ledgerFile . to show . packed
 , ""
 , "# Format used to parse dates"
 , "date-format = " <> c ^. dateFormat . to show . packed
 , ""
 , "# Algorithm used to find completions for account names. Possible values are:"
 , "#   - substrings: Every word in the search string has to occur somewhere in the account name"
 , "#   - fuzzy: All letters from the search string have to appear in the name in the same order"
 , "completion-engine = \"" <> c ^. matchAlgo . to showMatchAlgo <> "\""
 ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \c -> pure defaultConfig
    <**> (c .:? "file" <&> optionally ledgerFile)
    <**> (c .:? "date-format" <&> optionally dateFormat)
    <**> (c .:? "completion-engine" <&> optionally matchAlgo)


optionally :: ASetter s t b b -> Maybe b -> s -> t
optionally setter Nothing = over setter id
optionally setter (Just x) = set setter x

replaceLeadingTilde :: FilePath -> FilePath -> FilePath
replaceLeadingTilde ('~':'/':path) replacement = replacement ++ "/" ++ path
replaceLeadingTilde path _ = path

replaceLeadingTildeByHome :: FilePath -> IO FilePath
replaceLeadingTildeByHome path = replaceLeadingTilde path <$> getHomeDirectory

showMatchAlgo :: MatchAlgo -> Text
showMatchAlgo Fuzzy = "fuzzy"
showMatchAlgo Substrings = "substrings"
