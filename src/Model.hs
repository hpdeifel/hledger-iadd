{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Model where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time hiding (parseTime)
import qualified Hledger as HL

data Step = DateQuestion
          | DescriptionQuestion Day
          | AccountQuestion1 HL.Transaction
          | AccountQuestion2 HL.AccountName HL.Transaction
          | FinalQuestion HL.Transaction


nextStep :: Text -> Step -> IO (Either HL.Transaction Step)
nextStep entryText current = case current of
  DateQuestion -> parseTime entryText >>= \case
    Nothing -> return $ Right current -- TODO Show error
    Just day -> return $ Right (DescriptionQuestion day)
  DescriptionQuestion day -> return $ Right $
    AccountQuestion1 (HL.nulltransaction { HL.tdate = day
                                         , HL.tdescription = (T.unpack entryText)})
  AccountQuestion1 trans
    | T.null entryText -> return $ Right $ FinalQuestion trans
    | otherwise        -> return $ Right $
      AccountQuestion2 (T.unpack entryText) trans
  AccountQuestion2 name trans -> return $ Right $
    let newPosting = HL.post name (parseAmount entryText)
    in AccountQuestion1 (addPosting newPosting trans)

  FinalQuestion trans
    | entryText == "y" -> return $ Left trans
    | otherwise -> return $ Right $ AccountQuestion1 trans

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
  Just . T.pack . formatTime defaultTimeLocale "%d.%m.%y" <$> getCurrentTime
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
    [d] -> return $ Just $ fromGregorian currentYear currentMonth (parseInt d)
    [d,m] -> return $ Just $ fromGregorian currentYear (parseInt m) (parseInt d)
    [d,m,y] -> return $ Just $ fromGregorian (parseInt y) (parseInt m) (parseInt d)
    _ -> return Nothing

-- TODO: Handle failure
parseInt :: (Read a, Num a) => Text -> a
parseInt t = read $ T.unpack t

-- TODO Handle failure
parseAmount :: Text -> HL.Amount
parseAmount = HL.amountp' . T.unpack

addPosting :: HL.Posting -> HL.Transaction -> HL.Transaction
addPosting p t = t { HL.tpostings = p : HL.tpostings t }
