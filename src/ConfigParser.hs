{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, DeriveFunctor, ScopedTypeVariables #-}

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
       , Option
       , OptionArgument()
       ) where

import           Control.Applicative
import           Control.Applicative.Free
import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.Functor.Identity
import           Data.Monoid
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S
import qualified Text.Megaparsec as P
import           Text.Megaparsec hiding ((<|>), many, option, optional)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Error
import           Text.Megaparsec.Text

-- | Parse a config file from a 'Text'.
parseConfig :: FilePath -- ^ File path to use in error messages
            -> Text -- ^ The input test
            -> OptParser a -- ^ The parser to use
            -> Either ConfParseError a
parseConfig path input parser = case parse (assignmentList <* eof) path input of
  Left err -> Left $ SyntaxError err
  Right res -> runOptionParser res parser

-- | Parse a config file from an actual file in the filesystem.
parseConfigFile :: FilePath -- ^ Path to the file
                -> OptParser a -- ^ The parser to use
                -> IO (Either ConfParseError a)
parseConfigFile path parser = do
  input <- T.readFile path
  return $ parseConfig path input parser

-- | An option in the config file. Use 'option' as a smart constructor.
data Option a = Option
  { optParser :: Parser a
  , optType :: Text -- Something like "string" or "integer"
  , optName :: Text
  , optHelp :: Text
  , optDefault :: a
  , optDefaultTxt :: Text -- printed version of optDefault
  } deriving (Functor)

-- | The main parser type. Use 'option' and the 'Applicative' instance to create those.
type OptParser a = Ap Option a

-- | Errors that can occur during parsing. Use the 'Show' instance for printing.
data ConfParseError = SyntaxError (ParseError Char Dec)
                    | UnknownOption SourcePos Text
                    | TypeError (ParseError Char Dec)
  deriving (Eq)

instance Show ConfParseError where
  show (SyntaxError e) = parseErrorPretty e
  show (UnknownOption pos key) =
    show pos ++ ": Unknown option " ++ T.unpack key
  show (TypeError e) = parseErrorPretty e

-- | Class for supported option types.
--
-- At the moment, orphan instances are not supported
class OptionArgument a where
  mkParser :: (Text, Parser a)
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
             -> Parser a -- ^ Parser for the option
             -> OptParser a
customOption optName optDefault optDefaultTxt optHelp optType optParser = liftAp $ Option {..}

instance OptionArgument Int where
  mkParser = ("integer", parseNumber)
  printArgument = T.pack . show

instance OptionArgument Integer where
  mkParser = ("integer", parseNumber)
  printArgument = T.pack . show

instance OptionArgument String where
  mkParser = ("string",  many anyChar)
  printArgument = quote . T.pack

instance OptionArgument Text where
  mkParser = ("string",  T.pack <$> many anyChar)
  printArgument = quote

quote :: Text -> Text
quote x = "\"" <> escape x <> "\""
  where
    escape = T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

runOptionParser :: [Assignment] -> OptParser a -> Either ConfParseError a
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

parseOption :: OptParser a -> Assignment -> Either ConfParseError (OptParser a)
parseOption (Pure _) ass =
  Left $ UnknownOption (assignmentPosition ass) (assignmentKey ass)
parseOption (Ap opt rest) ass
  | optName opt == assignmentKey ass =
    let content = (valueContent $ assignmentValue ass)
        pos = (valuePosition $ assignmentValue ass)
    in case parseWithStart (optParser opt <* eof) pos content of
         Left e -> Left $ TypeError $ addErrorMessage e $
           "in " ++ T.unpack (optType opt) ++ " argument for option " ++ T.unpack (assignmentKey ass)
         Right res -> Right $ fmap ($ res) rest
  | otherwise = fmap (Ap opt) $ parseOption rest ass

  where testParse = Nothing

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

assignmentList :: Parser [Assignment]
assignmentList = whitespace *> many (assignment <* whitespace)

assignment :: Parser Assignment
assignment = do
  Assignment
    <$> getPosition <*> key <* whitespaceNoComment
    <*  char '=' <* whitespaceNoComment
    <*> value

key :: Parser Text
key = T.pack <$> some (alphaNumChar <|> char '_' <|> char '-')

value :: Parser AssignmentValue
value = AssignmentValue <$> getPosition <*> content <* whitespaceNoEOL <* (void eol <|> eof)

content :: Parser Text
content =  escapedString
       <|> bareString

bareString :: Parser Text
bareString = (T.strip . T.pack <$> some (noneOf ("#\n" :: String)))
  <?> "bare string"

escapedString :: Parser Text
escapedString = (T.pack <$> (char '"' *> many escapedChar <* char '"'))
                <?> "quoted string"
  where escapedChar =  char '\\' *> anyChar
                   <|> noneOf ("\"" :: String)

whitespace :: Parser ()
whitespace = skipMany $ (void $ oneOf (" \t\n" :: String)) <|> comment

whitespaceNoEOL :: Parser ()
whitespaceNoEOL = skipMany $ (void $ oneOf (" \t" :: String)) <|> comment

whitespaceNoComment :: Parser ()
whitespaceNoComment = skipMany $ oneOf (" \t" :: String)

comment :: Parser ()
comment = char '#' >> skipMany (noneOf ("\n" :: String))

parseWithStart :: (Stream s, ErrorComponent e)
               => Parsec e s a -> SourcePos -> s -> Either (ParseError (Token s) e) a
parseWithStart p pos = parse p' (sourceName pos)
  where p' = do setPosition pos; p

parseNumber :: Read a => Parser a
parseNumber = read <$> ((<>) <$> (P.option "" $ string "-") <*> some digitChar)

-- | Helper function brought over from parsec
addErrorMessage :: ParseError t Dec -> String -> ParseError t Dec
addErrorMessage e errorMsg = e { errorCustom = S.insert (DecFail errorMsg) (errorCustom e) }
