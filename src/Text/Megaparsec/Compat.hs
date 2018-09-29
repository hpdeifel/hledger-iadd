{-# LANGUAGE GADTs #-}

-- | Compatibility module to bridge the gap between megaparsec-5 and megaparsec-6
--
-- Import this instead of Text.Megaparsec, Text.Megaparsec.Char and
-- Text.Megaparsec.Text
module Text.Megaparsec.Compat
  (
  -- * Re-exports
    module Text.Megaparsec.Char
  , module Text.Megaparsec
  -- * Compatibility reimplementations
  , Parser
  , Dec
  , string
  -- * Custom error handling
  , CustomError
  , mkCustomError
  , addCustomError
  -- * Additional helpers
  , parseWithStart
  ) where

import qualified Data.Set as S
import           Data.Text (Text)
import           Text.Megaparsec.Char hiding (string)
import qualified Text.Megaparsec.Char as P
import qualified Data.List.NonEmpty as NE

import           Data.Set (Set)
import           Data.Void
import           Text.Megaparsec

-- | Custom error type for when no custom errors are needed
type Dec = Void


-- | Same as the type in Text.Megaparsec.Text from megaparsec-5
type Parser = Parsec Dec Text


-- | Custom error type that mimics FancyError of megaparsec-6 but retains
-- information about unexpected and expected tokens.
data CustomError e = CustomError
  (Maybe (ErrorItem Char))      -- unexpected
  (Set (ErrorItem Char))        -- expected
  e                             -- custom error data
  deriving (Eq, Show, Ord)

instance ShowErrorComponent e => ShowErrorComponent (CustomError e) where
  showErrorComponent (CustomError us es e) =
    parseErrorTextPretty (TrivialError undefined us es :: ParseError Char Void)
    ++ showErrorComponent e


-- | Wrap a custom error type into a 'ParseError'.
mkCustomError :: SourcePos -> e -> ParseError t (CustomError e)
mkCustomError pos custom = FancyError (neSingleton pos)
  (S.singleton (ErrorCustom (CustomError Nothing S.empty custom)))


-- | Add a custom error to an already existing error.
--
-- This retains the original information such as expected and unexpected tokens
-- as well as the source position.
addCustomError :: Ord e => ParseError Char (CustomError e) -> e -> ParseError Char (CustomError e)
addCustomError e custom = case e of
  TrivialError source us es ->
    FancyError source (S.singleton (ErrorCustom (CustomError us es custom)))
  FancyError source es ->
    FancyError source (S.insert (ErrorCustom (CustomError Nothing S.empty custom)) es)


-- | Like 'parse', but start at a specific source position instead of 0.
parseWithStart :: (Stream s, Ord e)
               => Parsec e s a -> SourcePos -> s -> Either (ParseError (Token s) e) a
parseWithStart p pos = parse p' (sourceName pos)
  where p' = do setPosition pos; p


-- | Reimplementation of 'Text.Megaparsec.Char.string', but specialized to 'Text'.
string :: MonadParsec e s m => Tokens s -> m (Tokens s)
string = P.string


neSingleton :: a -> NE.NonEmpty a
neSingleton x = x NE.:| []
