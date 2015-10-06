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


nextStep :: Text -> Step -> IO Step
nextStep entryText current = case current of
  DateQuestion -> parseTime entryText >>= \case
    Nothing -> return current -- TODO Show error
    Just day -> return (DescriptionQuestion day)
  DescriptionQuestion day -> return $
    AccountQuestion1 (HL.nulltransaction { HL.tdate = day
                                         , HL.tdescription = (T.unpack entryText)})
  AccountQuestion1 trans -> return $
    AccountQuestion2 (T.unpack entryText) trans
  AccountQuestion2 name trans -> return $
    let newPosting = HL.post name (parseAmount entryText)
    in AccountQuestion1 (addPosting newPosting trans)

context :: HL.Journal -> Text -> Step -> [Text]
context _ _ DateQuestion = []
context _ _ (DescriptionQuestion _) = []
context j entryText (AccountQuestion1 _) =
  let names = map T.pack $ HL.journalAccountNames j
  in if T.null entryText
     then []
     else filter (matches entryText) names
context _ _ (AccountQuestion2 _ _) = []

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
