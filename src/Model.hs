{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Model
       ( Step(..)
       , MaybeStep(..)
       , nextStep
       , context
       , suggest
       ) where

import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Time hiding (parseTime)
import qualified Hledger as HL

import           AmountParser

data Step = DateQuestion
          | DescriptionQuestion Day
          | AccountQuestion HL.Transaction
          | AmountQuestion HL.AccountName HL.Transaction
          | FinalQuestion HL.Transaction


data MaybeStep = Finished HL.Transaction
               | Step Step

nextStep :: HL.Journal -> Text -> Step -> IO (Either Text MaybeStep)
nextStep journal entryText current = case current of
  DateQuestion -> parseTime entryText >>= \case
    Nothing -> return $ Left "Could not parse date. Format: %d[.%m[.%Y]]"
    Just day -> return $ Right $ Step (DescriptionQuestion day)
  DescriptionQuestion day -> return $ Right $ Step $
    AccountQuestion HL.nulltransaction { HL.tdate = day
                                         , HL.tdescription = T.unpack entryText}
  AccountQuestion trans
    | T.null entryText -> return $ Right $ Step $ FinalQuestion trans
    | otherwise        -> return $ Right $ Step $
      AmountQuestion (T.unpack entryText) trans
  AmountQuestion name trans -> case parseAmount (HL.jContext journal) entryText of
    Left err -> return $ Left (T.pack err)
    Right amount -> return $ Right $ Step $
      let newPosting = post' name amount
      in AccountQuestion (addPosting newPosting trans)

  FinalQuestion trans
    | entryText == "y" -> return $ Right $ Finished trans
    | otherwise -> return $ Right $ Step $ AccountQuestion trans

context :: HL.Journal -> Text -> Step -> [Text]
context _ _ DateQuestion = []
context j entryText (DescriptionQuestion _) =
  let descs = map T.pack $ HL.journalDescriptions j
  in filterIfNotEmpty entryText matches descs
context j entryText (AccountQuestion _) =
  let names = map T.pack $ HL.journalAccountNames j
  in filterIfNotEmpty entryText matches names
context journal entryText (AmountQuestion _ _) =
  maybeToList $ T.pack . HL.showMixedAmount <$> trySumAmount (HL.jContext journal) entryText
context _ _  (FinalQuestion _) = []

filterIfNotEmpty :: Text -> (Text -> Text -> Bool) -> [Text] -> [Text]
filterIfNotEmpty t f l
  | T.null t = []
  | otherwise = filter (f t) l

suggest :: HL.Journal -> Step -> IO (Maybe Text)
suggest _ DateQuestion =
  Just . T.pack . formatTime defaultTimeLocale "%d.%m.%Y" <$> getCurrentTime
suggest _ (DescriptionQuestion _) = return Nothing
suggest journal (AccountQuestion trans) = return $
  case HL.transactionPostingBalances trans of
    (rsum, _, _)
      | HL.isZeroMixedAmount rsum && numPostings trans /= 0 -> Nothing
      | otherwise -> suggestAccount journal (numPostings trans) (T.pack $ HL.tdescription trans)
suggest journal (AmountQuestion _ trans) = return $ fmap (T.pack . HL.showMixedAmount) $
  case HL.transactionPostingBalances trans of
    (rsum, _, _)
      | HL.isZeroMixedAmount rsum -> suggestAmount journal (numPostings trans) (T.pack $ HL.tdescription trans)
      | otherwise -> Just $ HL.divideMixedAmount rsum (-1)
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

trySumAmount :: HL.JournalContext -> Text -> Maybe HL.MixedAmount
trySumAmount ctx = either (const Nothing) Just . parseAmount ctx

suggestAccount :: HL.Journal -> Int -> Text -> Maybe Text
suggestAccount journal num desc = T.pack <$>
  (nthAccountName num =<< findLastSimilar journal desc)

suggestAmount :: HL.Journal -> Int -> Text -> Maybe HL.MixedAmount
suggestAmount journal num desc = nthAmount num =<< findLastSimilar journal desc

findLastSimilar :: HL.Journal -> Text -> Maybe HL.Transaction
findLastSimilar journal desc =
  maximumBy (compare `on` HL.tdate) <$>
    listToMaybe' (filter ((==desc) . T.pack . HL.tdescription) $ HL.jtxns journal)

nthAccountName :: Int -> HL.Transaction -> Maybe HL.AccountName
nthAccountName num = safeIdx num . map HL.paccount . HL.tpostings

nthAmount :: Int -> HL.Transaction -> Maybe HL.MixedAmount
nthAmount num = safeIdx num . map HL.pamount . HL.tpostings

listToMaybe' :: [a] -> Maybe [a]
listToMaybe' [] = Nothing
listToMaybe' ls = Just ls

safeIdx :: Int -> [a] -> Maybe a
safeIdx _ [] = Nothing
safeIdx 0 (l:_) = Just l
safeIdx n (_:ls) = safeIdx (n-1) ls

numPostings :: HL.Transaction -> Int
numPostings = length . HL.tpostings
