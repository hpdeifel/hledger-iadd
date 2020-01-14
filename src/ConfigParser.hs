{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, DeriveFunctor, ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- | Applicative config parser.
--
-- This parses config files in the style of optparse-applicative. It supports
-- automatic generation of a default config both as datatype and in printed
-- form.
--
-- Example:
--
-- @
-- data Config = Config
--   { test :: Text
--   , foobar :: Int
--   }
--
-- confParser :: ConfParser Config
-- confParser = Config
--          \<$\> option "test" "default value" "Help for test"
--          \<*\> option "foobar" 42 "Help for foobar"
-- @
--
-- This parses a config file like the following:
--
-- > # This is a comment
-- > test = "something"
-- > foobar = 23
module ConfigParser
       ( OptParser
       , parseConfig
       , parseConfigFile
       , option
       , customOption
       , parserDefault
       , parserExample
       , ConfParseError
       , OParser
       , Option
       , OptionArgument()
       ) where

import           Control.Applicative hiding (many, some)
import           Control.Applicative.Free
import           Control.Monad
import           Data.Functor.Identity
import           Data.Semigroup ((<>))
import qualified Data.List.NonEmpty as NE

import qualified Data.Set as S
-- import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Megaparsec hiding (option)
import           Text.Megaparsec.Char
import           Data.Maybe
-- import           Text.Megaparsec.Text

-- | Errors that can occur during parsing. Use the 'Show' instance for printing.
data ConfParseError = UnknownOption Text
                    | TypeError Text Text -- Type and Option name
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ConfParseError where
  showErrorComponent (UnknownOption name) = "Unknown option " ++ T.unpack name
  showErrorComponent (TypeError typ name) =
    "in " ++ T.unpack typ ++ " argument for option " ++ T.unpack name

type OParser = Parsec ConfParseError Text

type CustomParseError = ParseErrorBundle Text ConfParseError

-- | Parse a config file from a 'Text'.
parseConfig :: FilePath -- ^ File path to use in error messages
            -> Text -- ^ The input test
            -> OptParser a -- ^ The parser to use
            -> Either CustomParseError a
parseConfig path input parser = case parse (assignmentList <* eof) path input of
  Left err -> Left err
  Right res -> runOptionParser res parser

-- | Parse a config file from an actual file in the filesystem.
parseConfigFile :: FilePath -- ^ Path to the file
                -> OptParser a -- ^ The parser to use
                -> IO (Either CustomParseError a)
parseConfigFile path parser = do
  input <- T.readFile path
  return $ parseConfig path input parser

-- | An option in the config file. Use 'option' as a smart constructor.
data Option a = Option
  { optParser :: OParser a
  , optType :: Text -- Something like "string" or "integer"
  , optName :: Text
  , optHelp :: Text
  , optDefault :: a
  , optDefaultTxt :: Text -- printed version of optDefault
  } deriving (Functor)

-- | The main parser type. Use 'option' and the 'Applicative' instance to create those.
type OptParser a = Ap Option a

-- | Class for supported option types.
--
-- At the moment, orphan instances are not supported
class OptionArgument a where
  mkParser :: (Text, OParser a)
  printArgument :: a -> Text

-- | 'OptParser' that parses one option.
--
-- Can be combined with the 'Applicative' instance for 'OptParser'. See the
-- module documentation for an example.
option :: OptionArgument a
       => Text -- ^ The option name
       -> a -- ^ The default value
       -> Text
          -- ^ A help string for the option. Will be used by 'parserExample' to
          -- create helpful comments.
       -> OptParser a
option name def help = liftAp $ Option parser typename name help def (printArgument def)
  where (typename, parser) = mkParser

customOption :: Text -- ^ The option name
             -> a -- ^ The default Value
             -> Text -- ^ A textual representation of the default value
             -> Text -- ^ A help string for the option
             -> Text -- ^ A description of the expected type such sas "string" or "integer"
             -> OParser a -- ^ Parser for the option
             -> OptParser a
customOption optName optDefault optDefaultTxt optHelp optType optParser = liftAp $ Option {..}

instance OptionArgument Int where
  mkParser = ("integer", parseNumber)
  printArgument = T.pack . show

instance OptionArgument Integer where
  mkParser = ("integer", parseNumber)
  printArgument = T.pack . show

instance OptionArgument String where
  mkParser = ("string",  many anySingle)
  printArgument = quote . T.pack

instance OptionArgument Text where
  mkParser = ("string",  T.pack <$> many anySingle)
  printArgument = quote

quote :: Text -> Text
quote x = "\"" <> escape x <> "\""
  where
    escape = T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

runOptionParser :: [Assignment] -> OptParser a -> Either CustomParseError a
runOptionParser (a:as) parser =  parseOption parser a >>= runOptionParser as
runOptionParser [] parser = Right $ parserDefault parser

-- | Returns the default value of a given parser.
--
-- This default value is computed from the default arguments of the 'option'
-- constructor. For the parser from the module description, the default value
-- would be:
--
-- > Config { test = "default value"
-- >        , foobar :: 42
-- >        }
parserDefault :: OptParser a -> a
parserDefault = runIdentity . runAp (Identity . optDefault)

-- | Generate the default config file.
--
-- This returns a valid config file, filled with the default values of every
-- option and using the help string of these options as comments.
parserExample :: OptParser a -> Text
parserExample = T.strip . runAp_ example1
  where example1 a = commentify (optHelp a) <> optName a <> " = " <> optDefaultTxt a <> "\n\n"
        commentify = T.unlines . map ("# " <>) . T.lines

parseOption :: OptParser a -> Assignment -> Either CustomParseError (OptParser a)
parseOption (Pure _) ass =
  Left $ mkCustomError (assignmentPosition ass) (UnknownOption (assignmentKey ass))
parseOption (Ap opt rest) ass
  | optName opt == assignmentKey ass =
    let content = (valueContent $ assignmentValue ass)
        pos = (valuePosition $ assignmentValue ass)
    in case parseWithStart (optParser opt <* eof) pos content of
         Left e -> Left $ addCustomError e $ TypeError (optType opt) (assignmentKey ass)
         Right res -> Right $ fmap ($ res) rest
  | otherwise = fmap (Ap opt) $ parseOption rest ass

mkCustomError :: SourcePos -> e -> ParseErrorBundle Text e
mkCustomError pos e = ParseErrorBundle
  { bundleErrors = NE.fromList [FancyError 0 (S.singleton (ErrorCustom e))]
  , bundlePosState = PosState
    { pstateInput = ""
    , pstateOffset = 0
    , pstateSourcePos = pos
    , pstateTabWidth = mkPos 1
    , pstateLinePrefix = ""
    }
  }
addCustomError :: ParseErrorBundle Text e -> e -> ParseErrorBundle Text e
addCustomError e customE =
  e { bundleErrors = NE.cons
                      (FancyError 0 (S.singleton (ErrorCustom customE)))
                      (bundleErrors e)}

-- Low level assignment parser

data Assignment = Assignment
  { assignmentPosition :: SourcePos
  , assignmentKey :: Text
  , assignmentValue :: AssignmentValue
  } deriving (Show)

data AssignmentValue = AssignmentValue
  { valuePosition :: SourcePos
  , valueContent :: Text
  } deriving (Show)

assignmentList :: OParser [Assignment]
assignmentList = whitespace *> many (assignment <* whitespace)

assignment :: OParser Assignment
assignment = do
  Assignment
    <$> getSourcePos <*> key <* whitespaceNoComment
    <*  char '=' <* whitespaceNoComment
    <*> value

key :: OParser Text
key = T.pack <$> some (alphaNumChar <|> char '_' <|> char '-')

value :: OParser AssignmentValue
value = AssignmentValue <$> getSourcePos <*> content <* whitespaceNoEOL <* (void eol <|> eof)

content :: OParser Text
content =  escapedString
       <|> bareString

bareString :: OParser Text
bareString = (T.strip . T.pack <$> some (noneOf ("#\n" :: String)))
  <?> "bare string"

escapedString :: OParser Text
escapedString = (T.pack <$> (char '"' *> many escapedChar <* char '"'))
                <?> "quoted string"
  where escapedChar =  char '\\' *> anySingle
                   <|> noneOf ("\"" :: String)

whitespace :: OParser ()
whitespace = skipMany $ (void $ oneOf (" \t\n" :: String)) <|> comment

whitespaceNoEOL :: OParser ()
whitespaceNoEOL = skipMany $ (void $ oneOf (" \t" :: String)) <|> comment

whitespaceNoComment :: OParser ()
whitespaceNoComment = skipMany $ oneOf (" \t" :: String)

comment :: OParser ()
comment = char '#' >> skipMany (noneOf ("\n" :: String))

parseNumber :: Read a => OParser a
parseNumber = read <$> ((<>) <$> (maybeToList <$> optional (char '-')) <*> some digitChar)


-- | Like 'parse', but start at a specific source position instead of 0.
parseWithStart :: (Stream s, Ord e)
               => Parsec e s a -> SourcePos -> s -> Either (ParseErrorBundle s e) a
parseWithStart p pos s = snd (runParser' p state)
  where state = State
          { stateInput = s
          , stateOffset = 0
          , statePosState =PosState
            { pstateInput = s
            , pstateOffset = 0
            , pstateSourcePos = pos
            , pstateTabWidth = mkPos 1
            , pstateLinePrefix = ""
            }
#if MIN_VERSION_megaparsec(8,0,0)
          , stateParseErrors = []
#endif
          }
