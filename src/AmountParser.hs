{-# LANGUAGE CPP #-}

module AmountParser (parseAmount) where

import           Data.Text (Text)
import qualified Hledger as HL
import           Data.Functor.Identity
import           Control.Monad.Trans.State.Strict
import           Text.Megaparsec.Compat hiding (Parser)

#if MIN_VERSION_hledger_lib(1,3,1)
type Parser a = HL.JournalParser Identity a
#else
type Parser a = HL.JournalStateParser Identity a
#endif

parseAmount :: HL.Journal -> Text -> Either String HL.MixedAmount
parseAmount journal t = case runIdentity $ runParserT (evalStateT (mixed <* optional space <* eof) journal) "" t of
  Left err -> Left (parseErrorPretty err)
  Right res -> Right res

mixed :: Parser HL.MixedAmount
mixed = HL.mixed <$> expr

expr :: Parser [HL.Amount]
expr = some (try $ lexeme factor)

factor :: Parser HL.Amount
factor =  (char '+' >> lexeme HL.amountp)
      <|> (char '-' >> flip HL.divideAmount (-1) <$> lexeme HL.amountp)
      <|> HL.amountp

lexeme :: Parser a -> Parser a
lexeme p = space >> p
