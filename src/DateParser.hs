{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TupleSections #-}
{-# LANGUAGE DeriveFunctor, LambdaCase, ViewPatterns #-}

module DateParser
       ( DateFormat
       , parseDateFormat
       , german

       , parseDate
       , parseDateWithToday

       , parseHLDate
       , parseHLDateWithToday

       , printDate

       -- * Utilities
       , weekDay
       ) where

import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Buildable (Buildable,build)
import           Data.Text.Format hiding (build)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder, toLazyText)
import           Data.Time hiding (parseTime)
import           Data.Time.Calendar.WeekDate
import qualified Hledger.Data.Dates as HL
import           Text.Parsec hiding ((<|>), many)
import           Text.Parsec.Text

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


parseHLDate :: Day -> Text -> Either Text Day
parseHLDate current text = case parse HL.smartdate "date" (T.unpack text) of
  Right res -> Right $ HL.fixSmartDate current res
  Left err -> Left $ T.pack $ show err

parseHLDateWithToday :: Text -> IO (Either Text Day)
parseHLDateWithToday text = flip parseHLDate text . utctDay <$> getCurrentTime

-- | Corresponds to %d[.[%m[.[%y]]]]
german :: DateFormat
german = DateFormat
  [ DateDay
  , DateOptional [DateString "."
                 ,DateOptional [DateMonth
                               ,DateOptional [DateString "."
                                             ,DateOptional [DateYearShort]]]]]

parseDateFormat :: Text -> Either Text DateFormat
parseDateFormat text = case parse dateSpec "date-format" text of
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
  let en = Just <$> parseEnglish current
      num = completeDate current . fmap getFirst <$> parseDate' spec <* eof

  in case parse ((try en <|> num) <* eof) "date" text of
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


-- Parses an english word such as 'yesterday' or 'monday'
parseEnglish :: Day -> Parser Day
parseEnglish current = ($ current) <$> choice (relativeDays ++ weekDays)

relativeDays :: [Parser (Day -> Day)]
relativeDays = map try
  [ addDays 1    <$ string "tomorrow"
  , id           <$ string "today"
  , addDays (-1) <$ string "yesterday"
  ]

weekDays :: [Parser (Day -> Day)]
weekDays = zipWith (\i name -> weekDay i <$ try (string name)) [1..]
  [ "monday"
  , "tuesday"
  , "wednesday"
  , "thursday"
  , "friday"
  , "saturday"
  , "sunday"
  ]

-- | Computes a relative date by the given weekday
--
-- Returns the first weekday with index wday, that's before the current date.
weekDay :: Int -> Day -> Day
weekDay wday current =
  let (_, _, wday') = toWeekDate current
      difference = negate $ (wday' - wday) `mod` 7
  in addDays (toInteger difference) current


printDate :: DateFormat -> Day -> Text
printDate (DateFormat spec) day = TL.toStrict $ toLazyText $ printDate' spec day

printDate' :: [DateSpec] -> Day -> Builder
printDate' [] _ = ""
printDate' (DateYear:ds) day@(toGregorian -> (y,_,_)) =
  build y <> printDate' ds day
printDate' (DateYearShort:ds) day@(toGregorian -> (y,_,_))
  | y > 2000  = twoDigits (y-2000) <> printDate' ds day
  | otherwise = twoDigits y <> printDate' ds day
printDate' (DateMonth:ds) day@(toGregorian -> (_,m,_)) =
  twoDigits m <> printDate' ds day
printDate' (DateDay:ds) day@(toGregorian -> (_,_,d)) =
  twoDigits d <> printDate' ds day
printDate' (DateString s:ds) day =
  build s <> printDate' ds day
printDate' (DateOptional opt:ds) day =
  printDate' opt day <> printDate' ds day

twoDigits :: Buildable a => a -> Builder
twoDigits = left 2 '0'
