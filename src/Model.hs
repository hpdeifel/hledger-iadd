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
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord (Down(..))
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
  in sortBy (descUses j) $ filter (entryText `matches`) descs
context j entryText (AccountQuestion _) =
  let names = map T.pack $ HL.journalAccountNames j
  in  filter (entryText `matches`) names
context journal entryText (AmountQuestion _ _) =
  maybeToList $ T.pack . HL.showMixedAmount <$> trySumAmount (HL.jContext journal) entryText
context _ _  (FinalQuestion _) = []

-- | Suggest the initial text of the entry box for each step
--
-- For example, it suggests today for the date prompt
suggest :: HL.Journal -> Step -> IO (Maybe Text)
suggest _ DateQuestion =
  Just . T.pack . formatTime defaultTimeLocale "%d.%m.%Y" <$> getCurrentTime
suggest _ (DescriptionQuestion _) = return Nothing
suggest journal (AccountQuestion trans) = return $
  if numPostings trans /= 0 && transactionBalanced trans
    then Nothing
    else T.pack . HL.paccount <$> (suggestNextPosting trans =<< findLastSimilar journal trans)
suggest journal (AmountQuestion account trans) = return $ fmap (T.pack . HL.showMixedAmount) $
  if transactionBalanced trans
    then HL.pamount <$> (findPostingByAcc account =<< findLastSimilar journal trans)
    else Just $ negativeAmountSum trans
suggest _ (FinalQuestion _) = return $ Just "y"

-- | Returns true if the pattern is not empty and all of its words occur in the string
--
-- If the pattern is empty, we don't want any entries in the list, so nothing is
-- selected if the users enters an empty string. Empty inputs are special cased,
-- so this is important.
matches :: Text -> Text -> Bool
matches a b
  | T.null a = False
  | otherwise = matches' (T.toCaseFold a) (T.toCaseFold b)
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


-- | Given a previous similar transaction, suggest the next posting to enter
--
-- This next posting is the one the user likely wants to type in next.
suggestNextPosting :: HL.Transaction -> HL.Transaction -> Maybe HL.Posting
suggestNextPosting current reference =
  -- Postings that aren't already used in the new posting
  let unusedPostings = filter (`notContainedIn` curPostings) refPostings
  in listToMaybe $ sortBy cmpPosting unusedPostings

  where [refPostings, curPostings] = map HL.tpostings [reference, current]
        notContainedIn p = not . any (((==) `on` HL.paccount) p)
        -- Sort descending by amount. This way, negative amounts rank last
        cmpPosting = compare `on` (Down . HL.pamount)

findLastSimilar :: HL.Journal -> HL.Transaction -> Maybe HL.Transaction
findLastSimilar journal desc =
  maximumBy (compare `on` HL.tdate) <$>
    listToMaybe' (filter (((==) `on` (T.pack . HL.tdescription)) desc) $ HL.jtxns journal)

-- | Return the first Posting that matches the given account name in the transaction
findPostingByAcc :: HL.AccountName -> HL.Transaction -> Maybe HL.Posting
findPostingByAcc account = find ((==account) . HL.paccount) . HL.tpostings

listToMaybe' :: [a] -> Maybe [a]
listToMaybe' [] = Nothing
listToMaybe' ls = Just ls

numPostings :: HL.Transaction -> Int
numPostings = length . HL.tpostings

-- | Returns True if all postings balance and the transaction is not empty
transactionBalanced :: HL.Transaction -> Bool
transactionBalanced trans =
  let (rsum, _, _) = HL.transactionPostingBalances trans
  in HL.isZeroMixedAmount rsum

-- | Computes the sum of all postings in the transaction and inverts it
negativeAmountSum :: HL.Transaction -> HL.MixedAmount
negativeAmountSum trans =
  let (rsum, _, _) = HL.transactionPostingBalances trans
  in HL.divideMixedAmount rsum (-1)

-- | Compare two transaction descriptions based on their number of occurences in
-- the given journal.
descUses :: HL.Journal -> Text -> Text -> Ordering
descUses journal = compare `on` (Down . flip M.lookup usesMap)
  where usesMap = foldr (count . T.pack . HL.tdescription) M.empty $
                  HL.jtxns journal
        -- Add one to the current count of this element
        count :: Text -> M.Map Text (Sum Int) -> M.Map Text (Sum Int)
        count = M.alter (<> Just 1)
