{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TupleSections #-}
{-# LANGUAGE DeriveFunctor, LambdaCase #-}

module DateParser
       ( DateFormat
       , parseDateFormat
       , german

       , parseDate
       , parseDateWithToday
       , parseDateOrHLDate
       ) where

import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time hiding (parseTime)
import           Text.Parsec hiding ((<|>), many)
import           Text.Parsec.Text
import qualified Hledger.Data.Dates as HL

newtype DateFormat = DateFormat [DateSpec]
                   deriving (Eq, Show)

-- TODO Add show instance that corresponds to parsed expression

data DateSpec = DateYear
              | DateYearShort
              | DateMonth
              | DateDay
              | DateString Text
              | DateOptional [DateSpec]
                deriving (Show, Eq)


-- | Try to parse date according to given DateSpec and then using hledgers
-- internal parsing, if the first fails.
parseDateOrHLDate :: DateFormat -> Text -> IO (Either Text Day)
parseDateOrHLDate spec text =
  parseDateWithToday spec text >>= \case
    Right res -> return $ Right res
    Left err -> case parse HL.smartdate "date" (T.unpack text) of
      Right res -> do
        today <- utctDay <$> getCurrentTime
        return $ Right $ HL.fixSmartDate today res
      Left _ -> return $ Left err


-- | Corresponds to %d[.[%m[.[%y]]]]
german :: DateFormat
german = DateFormat
  [ DateDay
  , DateOptional [DateString "."
                 ,DateOptional [DateMonth
                               ,DateOptional [DateString "."
                                             ,DateOptional [DateYearShort]]]]]

parseDateFormat :: Text -> Either Text DateFormat
parseDateFormat text = case parse dateSpec "input" text of
  Left err  -> Left $ T.pack $ show err
  Right res -> Right res

dateSpec :: Parser DateFormat
dateSpec = DateFormat <$> (many oneTok <* eof)

oneTok :: Parser DateSpec
oneTok =  char '%' *> percent
      <|> char '\\' *> escape
      <|> DateOptional <$> between (char '[') (char ']') (many oneTok)
      <|> DateString . T.pack <$> some (noneOf "\\[]%")

percent :: Parser DateSpec
percent =  char 'y' *> pure DateYearShort
       <|> char 'Y' *> pure DateYear
       <|> char 'm' *> pure DateMonth
       <|> char 'd' *> pure DateDay
       <|> char '%' *> pure (DateString "%")

escape :: Parser DateSpec
escape =  char '\\' *> pure (DateString "\\")
      <|> char '[' *> pure (DateString "[")
      <|> char ']' *> pure (DateString "]")

-- | Parse text with given format and fill in missing fields with todays date.
parseDateWithToday :: DateFormat -> Text -> IO (Either Text Day)
parseDateWithToday spec text = do
  today <- utctDay <$> getCurrentTime
  return (parseDate today spec text)

parseDate :: Day -> DateFormat -> Text -> Either Text Day
parseDate current (DateFormat spec) text =
  case completeDate current . fmap getFirst <$> parse (parseDate' spec <* eof) "date" text of
    Left err -> Left $ T.pack $ show err
    Right Nothing -> Left "Invalid Date"
    Right (Just d) -> Right d

-- (y, m, d)
newtype IncompleteDate a = IDate (a, a, a)
                       deriving (Monoid, Functor, Show)

completeDate :: Day  -> IncompleteDate (Maybe Int)-> Maybe Day
completeDate current (IDate (y,m,d)) =
  let (currentYear, currentMonth, currentDay) = toGregorian current
  in fromGregorianValid (fromMaybe currentYear (toInteger <$> y))
                        (fromMaybe currentMonth m)
                        (fromMaybe currentDay d)

parseDate' :: [DateSpec] -> Parser (IncompleteDate (First Int))
parseDate' [] = return mempty
parseDate' (d:ds) = case d of
  DateOptional sub -> try ((<>) <$> parseDate' sub <*> parseDate' ds)
                  <|> parseDate' ds

  _ -> (<>) <$> parseDate1 d <*> parseDate' ds


parseDate1 :: DateSpec -> Parser (IncompleteDate (First Int))
parseDate1 ds = case ds of
  DateYear      -> part (,mempty,mempty)
  DateYearShort -> part $ (,mempty,mempty) . fmap completeYear
  DateMonth     -> part (mempty,,mempty)
  DateDay       -> part (mempty,mempty,)
  DateString s  -> string (T.unpack s) >> pure mempty
  DateOptional ds' -> option mempty (try $ parseDate' ds')

  where digits = some digit
        part f = IDate . f . First . Just . (read :: String -> Int)  <$> digits
        completeYear year
          | year < 100 = year + 2000
          | otherwise  = year
