{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Model
       ( Step(..)
       , MaybeStep(..)
       , nextStep
       , undo
       , context
       , suggest
       ) where

import           Data.Function
import           Data.List
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe
import           Data.Monoid
import           Data.Ord (Down(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time hiding (parseTime)
import qualified Hledger as HL

import           AmountParser
import           DateParser

data Step = DateQuestion
          | DescriptionQuestion Day
          | AccountQuestion HL.Transaction
          | AmountQuestion HL.AccountName HL.Transaction
          | FinalQuestion HL.Transaction
          deriving (Eq, Show)


data MaybeStep = Finished HL.Transaction
               | Step Step
               deriving (Eq, Show)

nextStep :: HL.Journal -> DateFormat -> String -> Either Text Text -> Step -> IO (Either Text MaybeStep)
nextStep journal dateFormat defaultCurrency entryText current = case current of
  DateQuestion ->
    fmap (Step . DescriptionQuestion) <$> either (parseDateWithToday dateFormat)
                                                 parseHLDateWithToday
                                                 entryText
  DescriptionQuestion day -> return $ Right $ Step $
    AccountQuestion HL.nulltransaction { HL.tdate = day
                                       , HL.tdescription = T.unpack (fromEither entryText)}
  AccountQuestion trans
    | T.null (fromEither entryText) && transactionBalanced trans
      -> return $ Right $ Step $ FinalQuestion trans
    | T.null (fromEither entryText)  -- unbalanced
      -> return $ Left $ "Transaction not balanced! Please balance your transaction before adding it to the journal."
    | otherwise        -> return $ Right $ Step $
      AmountQuestion (T.unpack (fromEither entryText)) trans
  AmountQuestion name trans -> case parseAmountWithDefault (HL.jContext journal) defaultCurrency (fromEither entryText) of
    Left err -> return $ Left (T.pack err)
    Right amount -> return $ Right $ Step $
      let newPosting = post' name amount
      in AccountQuestion (addPosting newPosting trans)

  FinalQuestion trans
    | fromEither entryText == "y" -> return $ Right $ Finished trans
    | otherwise -> return $ Right $ Step $ AccountQuestion trans

-- | Reverses the last step.
--
-- Returns (Left errorMessage), if the step can't be reversed
undo :: Step -> Either Text Step
undo current = case current of
  DateQuestion -> Left "Already at oldest step in current transaction"
  DescriptionQuestion _ -> return DateQuestion
  AccountQuestion trans -> return $ case HL.tpostings trans of
    []     -> DescriptionQuestion (HL.tdate trans)
    ps -> AmountQuestion (HL.paccount (last ps)) trans { HL.tpostings = init ps }
  AmountQuestion _ trans -> Right $ AccountQuestion trans
  FinalQuestion trans -> undo (AccountQuestion trans)

context :: HL.Journal -> DateFormat -> String -> Text -> Step -> IO [Text]
context _ dateFormat _ entryText DateQuestion = parseDateWithToday dateFormat entryText >>= \case
  Left _ -> return []
  Right date -> return [T.pack $ HL.showDate date]
context j _ _ entryText (DescriptionQuestion _) = return $
  let descs = map T.pack $ HL.journalDescriptions j
  in sortBy (descUses j) $ filter (entryText `matches`) descs
context j _ _ entryText (AccountQuestion _) = return $
  let names = map T.pack $ HL.journalAccountNames j
  in  filter (entryText `matches`) names
context journal _ defaultCurrency entryText (AmountQuestion _ _) = return $
  maybeToList $ T.pack . HL.showMixedAmount <$> trySumAmount (HL.jContext journal) defaultCurrency entryText
context _ _ _ _  (FinalQuestion _) = return []

-- | Suggest the initial text of the entry box for each step
--
-- For example, it suggests today for the date prompt
suggest :: HL.Journal -> DateFormat -> Step -> IO (Maybe Text)
suggest _ dateFormat DateQuestion =
  Just . printDate dateFormat . utctDay <$> getCurrentTime
suggest _ _ (DescriptionQuestion _) = return Nothing
suggest journal _ (AccountQuestion trans) = return $
  if numPostings trans /= 0 && transactionBalanced trans
    then Nothing
    else T.pack . HL.paccount <$> (suggestAccountPosting journal trans)
suggest journal _ (AmountQuestion account trans) = return $ fmap (T.pack . HL.showMixedAmount) $
  if transactionBalanced trans
    then HL.pamount <$> (findPostingByAcc account =<< findLastSimilar journal trans)
    else Just $ negativeAmountSum trans
suggest _ _ (FinalQuestion _) = return $ Just "y"

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

post' :: HL.AccountName -> HL.MixedAmount -> HL.Posting
post' account amount = HL.nullposting { HL.paccount = account
                                      , HL.pamount = amount
                                      }

addPosting :: HL.Posting -> HL.Transaction -> HL.Transaction
addPosting p t = t { HL.tpostings = (HL.tpostings t) ++ [p] }

trySumAmount :: HL.JournalContext -> String -> Text -> Maybe HL.MixedAmount
trySumAmount ctx defaultCurrency = either (const Nothing) Just . parseAmountWithDefault ctx defaultCurrency


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

-- | Given the last transaction entered, suggest the likely most comparable posting
--
-- Since the transaction isn't necessarily the same type, we can't rely on matching the data
-- so we must use the order. This way if the user typically uses a certain order
-- like expense category and then payment method. Useful if entering many similar postings
-- in a row. For example, when entering transactions from a credit card statement
-- where the first account is usually food, and the second posting is always the credit card.
suggestCorrespondingPosting :: HL.Transaction -> HL.Transaction -> Maybe HL.Posting
suggestCorrespondingPosting current reference =
  let postingsEntered = length curPostings in
  if postingsEntered < (length refPostings) then
    Just (refPostings !! postingsEntered)
  else
    suggestNextPosting current reference
  where [refPostings, curPostings] = map HL.tpostings [reference, current]

findLastSimilar :: HL.Journal -> HL.Transaction -> Maybe HL.Transaction
findLastSimilar journal desc =
  maximumBy (compare `on` HL.tdate) <$>
    listToMaybe' (filter (((==) `on` (T.pack . HL.tdescription)) desc) $ HL.jtxns journal)

suggestAccountPosting :: HL.Journal -> HL.Transaction -> Maybe HL.Posting
suggestAccountPosting journal trans =
  case findLastSimilar journal trans of
    Just t -> suggestNextPosting trans t
    Nothing -> (last <$> listToMaybe' (HL.jtxns journal)) >>= (suggestCorrespondingPosting trans)

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
descUses journal = compare `on` (Down . flip HM.lookup usesMap)
  where usesMap = foldr (count . T.pack . HL.tdescription) HM.empty $
                  HL.jtxns journal
        -- Add one to the current count of this element
        count :: Text -> HM.HashMap Text (Sum Int) -> HM.HashMap Text (Sum Int)
        count = HM.alter (<> Just 1)

fromEither :: Either a a -> a
fromEither = either id id
