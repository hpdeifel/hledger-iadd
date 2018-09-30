{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module AmountParser (parseAmount) where

import           Data.Text                      ( Text )
import qualified Hledger                       as HL
import           Data.Functor.Identity
import           Control.Monad.Trans.State.Strict
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer    as L

#if MIN_VERSION_hledger_lib(1,3,1)
type Parser a = HL.JournalParser Identity a
#else
type Parser a = HL.JournalStateParser Identity a
#endif


parseAmount :: HL.Journal -> Text -> Either String HL.MixedAmount
parseAmount journal t =
  case
      runIdentity
        $ runParserT (evalStateT (expr <* space <* eof) journal) "" t
    of
      Left  err -> Left (parseErrorPretty err)
      Right res -> Right res


expr :: Parser HL.MixedAmount
expr = (evaluate <$> exprParser) >>= \case
  Left  err -> fail err
  Right res -> return res


symbol :: Text -> Parser Text
symbol = L.symbol space


data Expr = Amount HL.Amount
          | Negate Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr


-- NOTE Multiplication is a hack. It multiplies the quantities of two amounts if
-- the commodity is the same, allowing you to write 3€ * 3€ and not get €².
evaluate :: Expr -> Either String HL.MixedAmount
evaluate (Amount a ) = return (HL.mixed [a])
evaluate (Negate e ) = scaleAmount (-1) <$> evaluate e
evaluate (Add e1 e2) = concatAmounts <$> evaluate e1 <*> evaluate e2
evaluate (Sub e1 e2) = concatAmounts <$> evaluate e1 <*> evaluate (Negate e2)
evaluate (Mul e1 e2) = (,) <$> evaluate e1 <*> evaluate e2 >>= \case
  (e1', e2')
    | Just q <- isSimpleAndEqual e1' e2' -> return $ scaleAmount q e2'
    | Just q <- isSimpleAndEqual e2' e1' -> return $ scaleAmount q e1'
    | otherwise -> Left "Cannot multiply two amounts with different commodity"


exprParser :: Parser Expr
exprParser = makeExprParser term ops
  where
    ops =
      [ [Prefix (Negate <$ symbol "-"), Prefix (id <$ symbol "+")]
      , [InfixL (Mul <$ symbol "*")]
      , [InfixL (Add <$ symbol "+"), InfixL (Sub <$ symbol "-")]
      ]

    term = parens exprParser <|> (Amount <$> (HL.amountp <* space))


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


scaleAmount :: HL.Quantity -> HL.MixedAmount -> HL.MixedAmount
scaleAmount factor (HL.Mixed as) = HL.mixed (map scale as)
  where scale a = a { HL.aquantity = HL.aquantity a * factor }


concatAmounts :: HL.MixedAmount -> HL.MixedAmount -> HL.MixedAmount
concatAmounts (HL.Mixed a1) (HL.Mixed a2) = HL.mixed (a1 ++ a2)


isSimpleAndEqual :: HL.MixedAmount -> HL.MixedAmount -> Maybe HL.Quantity
isSimpleAndEqual (HL.Mixed [a]) (HL.Mixed [b])
  | HL.acommodity a == "" || HL.acommodity a == HL.acommodity b = Just
    (HL.aquantity a)
isSimpleAndEqual _ _ = Nothing
