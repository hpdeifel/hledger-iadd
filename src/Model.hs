{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Model where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Time hiding (parseTime)
import qualified Hledger as HL
import           Text.Parsec

import AmountParser

data Step = DateQuestion
          | DescriptionQuestion Day
          | AccountQuestion1 HL.Transaction
          | AccountQuestion2 HL.AccountName HL.Transaction
          | FinalQuestion HL.Transaction


data MaybeStep = Finished HL.Transaction
               | Step Step

nextStep :: Text -> Step -> IO (Either Text MaybeStep)
nextStep entryText current = case current of
  DateQuestion -> parseTime entryText >>= \case
    Nothing -> return $ Left "Could not parse date. Format: %d[.%m[.%Y]]"
    Just day -> return $ Right $ Step (DescriptionQuestion day)
  DescriptionQuestion day -> return $ Right $ Step $
    AccountQuestion1 (HL.nulltransaction { HL.tdate = day
                                         , HL.tdescription = (T.unpack entryText)})
  AccountQuestion1 trans
    | T.null entryText -> return $ Right $ Step $ FinalQuestion trans
    | otherwise        -> return $ Right $ Step $
      AccountQuestion2 (T.unpack entryText) trans
  AccountQuestion2 name trans -> case parseAmount entryText of
    Left err -> return $ Left (T.pack err)
    Right amount -> return $ Right $ Step $
      let newPosting = post' name amount
      in AccountQuestion1 (addPosting newPosting trans)

  FinalQuestion trans
    | entryText == "y" -> return $ Right $ Finished trans
    | otherwise -> return $ Right $ Step $ AccountQuestion1 trans

context :: HL.Journal -> Text -> Step -> [Text]
context _ _ DateQuestion = []
context j entryText (DescriptionQuestion _) =
  let descs = map T.pack $ HL.journalDescriptions j
  in filterIfNotEmpty entryText matches descs
context j entryText (AccountQuestion1 _) =
  let names = map T.pack $ HL.journalAccountNames j
  in filterIfNotEmpty entryText matches names
context _ _ (AccountQuestion2 _ _) = []
context _ _  (FinalQuestion _) = []

filterIfNotEmpty t f l
  | T.null t = []
  | otherwise = filter (f t) l

suggest :: HL.Journal -> Step -> IO (Maybe Text)
suggest _ DateQuestion =
  Just . T.pack . formatTime defaultTimeLocale "%d.%m.%Y" <$> getCurrentTime
suggest _ (DescriptionQuestion _) = return Nothing
suggest _ (AccountQuestion1 _) = return Nothing
suggest _ (AccountQuestion2 _ trans) =
  let (rsum, _, _) = HL.transactionPostingBalances trans
  in return $ Just $ T.pack $ HL.showMixedAmount (HL.divideMixedAmount rsum (-1))
suggest _ (FinalQuestion _) = return $ Just "y"

matches :: Text -> Text -> Bool
matches a b = matches' (T.toCaseFold a) (T.toCaseFold b)
  where matches' a' b' = all (`T.isInfixOf` b') (T.words a')

parseTime :: Text -> IO (Maybe Day)
parseTime t = do
  (currentYear, currentMonth, _) <- toGregorian . utctDay <$> getCurrentTime
  case filter (not . T.null) $ T.splitOn "." t of
    [d] -> return $ fromGregorian currentYear currentMonth <$> parseInt d
    [d,m] -> return $ fromGregorian currentYear <$> parseInt m <*> parseInt d
    [d,m,y] -> return $ fromGregorian <$> parseInt y <*> parseInt m <*> parseInt d
    _ -> return Nothing

parseInt :: (Read a, Integral a) => Text -> Maybe a
parseInt t = either (const Nothing) (Just . fst) $ T.decimal t

post' :: HL.AccountName -> HL.MixedAmount -> HL.Posting
post' account amount = HL.nullposting { HL.paccount = account
                                      , HL.pamount = amount
                                      }

addPosting :: HL.Posting -> HL.Transaction -> HL.Transaction
addPosting p t = t { HL.tpostings = p : HL.tpostings t }
