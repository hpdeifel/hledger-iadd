module AmountParser (parseAmount) where

import           Data.Text (Text)
import qualified Hledger as HL
import           Data.Functor.Identity
import           Control.Monad.Trans.State.Strict
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser a = HL.JournalParser Identity a

parseAmount :: HL.Journal -> Text -> Either String HL.MixedAmount
parseAmount journal t = case runIdentity $ runParserT (evalStateT (mixed <* optional space <* eof) journal) "" t of
  Left err -> Left (errorBundlePretty err)
  Right res -> Right res

mixed :: Parser HL.MixedAmount
mixed = HL.mixed <$> expr

expr :: Parser [HL.Amount]
expr = some (try $ lexeme factor)

factor :: Parser HL.Amount
factor =  (char '+' >> lexeme HL.amountp)
      <|> (char '-' >> HL.divideAmount (-1) <$> lexeme HL.amountp)
      <|> HL.amountp

lexeme :: Parser a -> Parser a
lexeme p = space >> p
