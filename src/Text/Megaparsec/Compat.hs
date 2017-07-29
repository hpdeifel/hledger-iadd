{-# LANGUAGE CPP, GADTs #-}

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
#if MIN_VERSION_megaparsec(6,0,0)
  , Dec
#endif
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

#if MIN_VERSION_megaparsec(6,0,0)
import           Data.Void
import           Text.Megaparsec
#else
import           Text.Megaparsec.Prim
import           Text.Megaparsec hiding (string)
import qualified Data.Text as T
#endif

#if MIN_VERSION_megaparsec(6,0,0)
-- | Custom error type for when no custom errors are needed
type Dec = Void
#endif

-- | Same as the type in Text.Megaparsec.Text from megaparsec-5
type Parser = Parsec Dec Text

-- | Custom error type that mimics FancyError of megaparsec-6 but retains
-- information about unexpected and expected tokens.
#if MIN_VERSION_megaparsec(6,0,0)
data CustomError e = CustomError
  { customError :: e  -- TODO Actually save (un)expected tokens on megaparsec-6
  } deriving (Eq, Show, Ord)

instance ShowErrorComponent e => ShowErrorComponent (CustomError e) where
  showErrorComponent (CustomError e) = showErrorComponent e
#else
data CustomError e = CustomError e
                   | CustomFail String
                   | CustomIndentation Ordering Pos Pos
  deriving (Eq, Ord, Show)

instance Ord e => ErrorComponent (CustomError e) where
  representFail = CustomFail
  representIndentation = CustomIndentation

instance ShowErrorComponent e => ShowErrorComponent (CustomError e) where
  showErrorComponent (CustomError e) = showErrorComponent e
  showErrorComponent (CustomFail msg) = msg
  showErrorComponent (CustomIndentation ord ref actual) =
    "incorrect indentation (got " ++ show (unPos actual) ++
    ", should be " ++ p ++ show (unPos ref) ++ ")"
    where p = case ord of
                LT -> "less than "
                EQ -> "equal to "
                GT -> "greater than "
#endif

-- | Wrap a custom error type into a 'ParseError'.
mkCustomError :: SourcePos -> e -> ParseError t (CustomError e)
#if MIN_VERSION_megaparsec(6,0,0)
mkCustomError pos custom = FancyError (neSingleton pos)
  (S.singleton (ErrorCustom (CustomError custom)))
#else
mkCustomError pos custom = ParseError (neSingleton pos) S.empty S.empty
  (S.singleton (CustomError custom))
#endif

-- | Add a custom error to an already existing error.
--
-- This retains the original information such as expected and unexpected tokens
-- as well as the source position.
addCustomError :: Ord e => ParseError t (CustomError e) -> e -> ParseError t (CustomError e)
#if MIN_VERSION_megaparsec(6,0,0)
addCustomError e custom = case e of
  TrivialError source _ _ -> FancyError source (S.singleton (ErrorCustom (CustomError custom)))
  FancyError source _ -> FancyError source (S.singleton (ErrorCustom (CustomError custom)))
#else
addCustomError e custom = e { errorCustom = S.insert (CustomError custom) (errorCustom e) }
#endif


-- | Like 'parse', but start at a specific source position instead of 0.
#if MIN_VERSION_megaparsec(6,0,0)
parseWithStart :: (Stream s, Ord e)
#else
parseWithStart :: (Stream s, ErrorComponent e)
#endif
               => Parsec e s a -> SourcePos -> s -> Either (ParseError (Token s) e) a
parseWithStart p pos = parse p' (sourceName pos)
  where p' = do setPosition pos; p


-- | Reimplementation of 'Text.Megaparsec.Char.string', but specialized to 'Text'.
#if MIN_VERSION_megaparsec(6,0,0)
string :: MonadParsec e s m => Tokens s -> m (Tokens s)
string = P.string
#else
string :: (MonadParsec e s m, Token s ~ Char) => Text -> m Text
string x = T.pack <$> P.string (T.unpack x)
#endif

neSingleton :: a -> NE.NonEmpty a
neSingleton x = x NE.:| []
