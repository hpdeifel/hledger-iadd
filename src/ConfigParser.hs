{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, DeriveFunctor, ScopedTypeVariables #-}

module ConfigParser
       ( Option
       , option
       , ConfParseError
       , OptParser
       , parseConfig
       , parseConfigFile
       , parserDefault
       , parserExample
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
import qualified Text.Parsec as P
import           Text.Parsec hiding ((<|>), many, option, optional)
import           Text.Parsec.Char
import           Text.Parsec.Error
import           Text.Parsec.Text

parseConfig :: FilePath -> Text -> OptParser a -> Either ConfParseError a
parseConfig path input parser = case parse (assignmentList <* eof) path input of
  Left err -> Left $ SyntaxError err
  Right res -> runOptionParser res parser

parseConfigFile :: FilePath -> OptParser a -> IO (Either ConfParseError a)
parseConfigFile path parser = do
  input <- T.readFile path
  return $ parseConfig path input parser

data Option a = Option
  { optParser :: Parser a
  , optType :: Text -- Something like "string" or "integer"
  , optName :: Text
  , optHelp :: Text
  , optDefault :: a
  , optDefaultTxt :: Text -- printed version of optDefault
  } deriving (Functor)

type OptParser a = Ap Option a

data ConfParseError = SyntaxError ParseError
                    | UnknownOption SourcePos Text
                    | TypeError ParseError
  deriving (Eq)

instance Show ConfParseError where
  show (SyntaxError e) = show e
  show (UnknownOption pos key) =
    show pos ++ ": Unknown option " ++ T.unpack key
  show (TypeError e) = show e

class OptionArgument a where
  mkParser :: (Text, Parser a)
  printArgument :: a -> Text

option :: OptionArgument a => Text -> a -> Text -> OptParser a
option name def help = liftAp $ Option parser typename name help def (printArgument def)
  where (typename, parser) = mkParser

parseNumber :: Read a => Parser a
parseNumber = read <$> ((<>) <$> (P.option "" $ string "-") <*> many1 digit)

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

parserDefault :: OptParser a -> a
parserDefault = runIdentity . runAp (Identity . optDefault)

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
         Left e -> Left $ TypeError $ flip addErrorMessage e $ Message $
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
key = T.pack <$> many1 (alphaNum <|> char '_' <|> char '-')

value :: Parser AssignmentValue
value = AssignmentValue <$> getPosition <*> content <* whitespaceNoEOL <* (void (endOfLine) <|> eof)

content :: Parser Text
content =  escapedString
       <|> bareString

bareString :: Parser Text
bareString = (T.strip . T.pack <$> many1 (noneOf "#\n"))
  <?> "bare string"

escapedString :: Parser Text
escapedString = (T.pack <$> (char '"' *> many escapedChar <* char '"'))
                <?> "quoted string"
  where escapedChar =  char '\\' *> anyChar
                   <|> noneOf "\""

whitespace :: Parser ()
whitespace = skipMany $ (void $ oneOf " \t\n") <|> comment

whitespaceNoEOL :: Parser ()
whitespaceNoEOL = skipMany $ (void $ oneOf " \t") <|> comment

whitespaceNoComment :: Parser ()
whitespaceNoComment = skipMany $ oneOf " \t"

comment :: Parser ()
comment = char '#' >> skipMany (noneOf "\n")

parseWithStart :: Stream s Identity t => Parsec s () a -> SourcePos -> s -> Either ParseError a
parseWithStart p pos = parse p' (sourceName pos)
  where p' = do setPosition pos; p
