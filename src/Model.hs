{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Model
       ( Step(..)
       , MaybeStep(..)
       , MatchAlgo(..)
       , nextStep
       , undo
       , context
       , suggest
       , setCurrentComment
       , getCurrentComment
       , setTransactionComment
       , getTransactionComment

       -- * Helpers exported for easier testing
       , accountsByFrequency
       , isDuplicateTransaction
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

type Comment = Text
type Duplicate = Bool

data Step = DateQuestion Comment
          | DescriptionQuestion Day Comment
          | AccountQuestion HL.Transaction Comment
          | AmountQuestion HL.AccountName HL.Transaction Comment
          | FinalQuestion HL.Transaction Duplicate
          deriving (Eq, Show)


data MaybeStep = Finished HL.Transaction
               | Step Step
               deriving (Eq, Show)

data MatchAlgo = Fuzzy | Substrings
  deriving (Eq, Show)

nextStep :: HL.Journal -> DateFormat -> Either Text Text -> Step -> IO (Either Text MaybeStep)
nextStep journal dateFormat entryText current = case current of
  DateQuestion comment ->
    fmap (Step . flip DescriptionQuestion comment)
       <$> either (parseDateWithToday dateFormat) parseHLDateWithToday entryText

  DescriptionQuestion day comment -> return $ Right $ Step $
    AccountQuestion HL.nulltransaction { HL.tdate = day
                                       , HL.tdescription = (fromEither entryText)
                                       , HL.tcomment = comment
                                       }
                                       "" -- empty comment
  AccountQuestion trans comment
    | T.null (fromEither entryText) && transactionBalanced trans
      -> return $ Right $ Step $ FinalQuestion trans (isDuplicateTransaction journal trans)
    | T.null (fromEither entryText)  -- unbalanced
      -> return $ Left $ "Transaction not balanced! Please balance your transaction before adding it to the journal."
    | otherwise        -> return $ Right $ Step $
      AmountQuestion (fromEither entryText) trans comment
  AmountQuestion name trans comment -> case parseAmount journal (fromEither entryText) of
    Left err -> return $ Left (T.pack err)
    Right amount -> return $ Right $ Step $
      let newPosting = post' name amount comment
      in AccountQuestion (addPosting newPosting trans) ""

  FinalQuestion trans _
    | fromEither entryText == "y" -> return $ Right $ Finished trans
    | otherwise -> return $ Right $ Step $ AccountQuestion trans ""

-- | Reverses the last step.
--
-- Returns (Left errorMessage), if the step can't be reversed
undo :: Step -> Either Text Step
undo current = case current of
  DateQuestion _ -> Left "Already at oldest step in current transaction"
  DescriptionQuestion _ comment -> return (DateQuestion comment)
  AccountQuestion trans _ -> return $ case HL.tpostings trans of
    []     -> DescriptionQuestion (HL.tdate trans) (HL.tcomment trans)
    ps -> AmountQuestion (HL.paccount (last ps)) trans { HL.tpostings = init ps } (HL.pcomment (last ps))
  AmountQuestion _ trans comment -> Right $ AccountQuestion trans comment
  FinalQuestion trans _ -> undo (AccountQuestion trans "")

context :: HL.Journal -> MatchAlgo -> DateFormat -> Text -> Step -> IO [Text]
context _ _ dateFormat entryText (DateQuestion _) = parseDateWithToday dateFormat entryText >>= \case
  Left _ -> return []
  Right date -> return [T.pack $ HL.showDate date]
context j matchAlgo _ entryText (DescriptionQuestion _ _) = return $
  let descs = HL.journalDescriptions j
  in sortBy (descUses j) $ filter (matches matchAlgo entryText) descs
context j matchAlgo _ entryText (AccountQuestion _ _) = return $
  let names = accountsByFrequency j
  in  filter (matches matchAlgo entryText) names
context journal _ _ entryText (AmountQuestion _ _ _) = return $
  maybeToList $ T.pack . HL.showMixedAmount <$> trySumAmount journal entryText
context _ _ _ _  (FinalQuestion _ _) = return []

-- | Suggest the initial text of the entry box for each step
--
-- For example, it suggests today for the date prompt
suggest :: HL.Journal -> DateFormat -> Step -> IO (Maybe Text)
suggest _ dateFormat (DateQuestion _) =
  Just . printDate dateFormat . utctDay <$> getCurrentTime
suggest _ _ (DescriptionQuestion _ _) = return Nothing
suggest journal _ (AccountQuestion trans _) = return $
  if numPostings trans /= 0 && transactionBalanced trans
    then Nothing
    else HL.paccount <$> (suggestAccountPosting journal trans)
suggest journal _ (AmountQuestion account trans _) = return $ fmap (T.pack . HL.showMixedAmount) $ do
  case findLastSimilar journal trans of
    Nothing
      | null (HL.tpostings trans)
        -> Nothing  -- Don't suggest an amount for first account
      | otherwise
        -> Just $ negativeAmountSum trans
    Just last
      | transactionBalanced trans || (trans `isSubsetTransaction` last)
        -> HL.pamount <$> (findPostingByAcc account last)
      | otherwise
        -> Just $ negativeAmountSum trans
suggest _ _ (FinalQuestion _ _) = return $ Just "y"

getCurrentComment :: Step -> Comment
getCurrentComment step = case step of
  DateQuestion c -> c
  DescriptionQuestion _ c -> c
  AccountQuestion _ c -> c
  AmountQuestion _ _ c -> c
  FinalQuestion trans _ -> HL.tcomment trans

setCurrentComment :: Comment -> Step -> Step
setCurrentComment comment step = case step of
  DateQuestion _ -> DateQuestion comment
  DescriptionQuestion date _ -> DescriptionQuestion date comment
  AccountQuestion trans _ -> AccountQuestion trans comment
  AmountQuestion trans name _ -> AmountQuestion trans name comment
  FinalQuestion trans duplicate -> FinalQuestion trans { HL.tcomment = comment } duplicate

getTransactionComment :: Step -> Comment
getTransactionComment step = case step of
  DateQuestion c -> c
  DescriptionQuestion _ c -> c
  AccountQuestion trans _ -> HL.tcomment trans
  AmountQuestion _ trans _ -> HL.tcomment trans
  FinalQuestion trans _ -> HL.tcomment trans

setTransactionComment :: Comment -> Step -> Step
setTransactionComment comment step = case step of
  DateQuestion _ -> DateQuestion comment
  DescriptionQuestion date _ -> DescriptionQuestion date comment
  AccountQuestion trans comment' ->
    AccountQuestion (trans { HL.tcomment = comment }) comment'
  AmountQuestion name trans comment' ->
    AmountQuestion name (trans { HL.tcomment = comment }) comment'
  FinalQuestion trans duplicate -> FinalQuestion trans { HL.tcomment = comment } duplicate

-- | Returns true if the pattern is not empty and all of its words occur in the string
--
-- If the pattern is empty, we don't want any entries in the list, so nothing is
-- selected if the users enters an empty string. Empty inputs are special cased,
-- so this is important.
matches :: MatchAlgo -> Text -> Text -> Bool
matches algo a b
  | T.null a = False
  | otherwise = matches' (T.toCaseFold a) (T.toCaseFold b)
  where
    matches' a' b'
      | algo == Fuzzy && T.any (== ':') b' = all (`fuzzyMatch` (T.splitOn ":" b')) (T.words a')
      | otherwise = all (`T.isInfixOf` b') (T.words a')

fuzzyMatch :: Text -> [Text] -> Bool
fuzzyMatch _ [] = False
fuzzyMatch query (part : partsRest) = case (T.uncons query) of
  Nothing -> True
  Just (c, queryRest)
    | c == ':' -> fuzzyMatch queryRest partsRest
    | otherwise -> fuzzyMatch query partsRest || case (T.uncons part) of
      Nothing -> False
      Just (c2, partRest)
        | c == c2 -> fuzzyMatch queryRest (partRest : partsRest)
        | otherwise -> False

post' :: HL.AccountName -> HL.MixedAmount -> Comment -> HL.Posting
post' account amount comment = HL.nullposting
  { HL.paccount = account
  , HL.pamount = amount
  , HL.pcomment = comment
  }

addPosting :: HL.Posting -> HL.Transaction -> HL.Transaction
addPosting p t = t { HL.tpostings = (HL.tpostings t) ++ [p] }

trySumAmount :: HL.Journal -> Text -> Maybe HL.MixedAmount
trySumAmount ctx = either (const Nothing) Just . parseAmount ctx


-- | Given a previous similar transaction, suggest the next posting to enter
--
-- This next posting is the one the user likely wants to type in next.
suggestNextPosting :: HL.Transaction -> HL.Transaction -> Maybe HL.Posting
suggestNextPosting current reference =
  -- Postings that aren't already used in the new posting
  let unusedPostings = filter (`notContainedIn` curPostings) refPostings
  in listToMaybe unusedPostings

  where [refPostings, curPostings] = map HL.tpostings [reference, current]
        notContainedIn p = not . any (((==) `on` HL.paccount) p)

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
    listToMaybe' (filter (((==) `on` HL.tdescription) desc) $ HL.jtxns journal)

suggestAccountPosting :: HL.Journal -> HL.Transaction -> Maybe HL.Posting
suggestAccountPosting journal trans =
  case findLastSimilar journal trans of
    Just t -> suggestNextPosting trans t
    Nothing -> (last <$> listToMaybe' (HL.jtxns journal)) >>= (suggestCorrespondingPosting trans)

-- | Return the first Posting that matches the given account name in the transaction
findPostingByAcc :: HL.AccountName -> HL.Transaction -> Maybe HL.Posting
findPostingByAcc account = find ((==account) . HL.paccount) . HL.tpostings

-- | Returns True if the first transaction is a subset of the second one.
--
-- That means, all postings from the first transaction are present in the
-- second one.
isSubsetTransaction :: HL.Transaction -> HL.Transaction -> Bool
isSubsetTransaction current origin =
  let
    origPostings = HL.tpostings origin
    currPostings = HL.tpostings current
  in
    null (deleteFirstsBy cmpPosting currPostings origPostings)
  where
    cmpPosting a b =  HL.paccount a == HL.paccount b
                   && HL.pamount a  == HL.pamount b


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
  where usesMap = foldr (count . HL.tdescription) HM.empty $
                  HL.jtxns journal
        -- Add one to the current count of this element
        count :: Text -> HM.HashMap Text (Sum Int) -> HM.HashMap Text (Sum Int)
        count = HM.alter (<> Just 1)

-- | All accounts occuring in the journal sorted in descending order of
-- appearance.
accountsByFrequency :: HL.Journal -> [HL.AccountName]
accountsByFrequency journal =
  let
    usedAccounts = map HL.paccount (HL.journalPostings journal)
    frequencyMap :: HM.HashMap HL.AccountName Int = foldr insertOrPlusOne HM.empty usedAccounts
    mapWithSubaccounts = foldr insertIfNotPresent frequencyMap (subaccounts frequencyMap)
    declaredAccounts = HL.expandAccountNames (HL.jaccounts journal)
    mapWithDeclared = foldr insertIfNotPresent mapWithSubaccounts declaredAccounts
  in
    map fst (sortBy (compare `on` (Down . snd)) (HM.toList mapWithDeclared))


  where
    insertOrPlusOne = HM.alter (Just . maybe 1 (+1))
    insertIfNotPresent account = HM.insertWith (flip const) account 0
    subaccounts m = HL.expandAccountNames (HM.keys m)

-- | Deterimine if a given transaction already occurs in the journal
--
-- This function ignores certain attributes of transactions, postings and
-- amounts that are either artifacts of knot-tying or are purely for
-- presentation.
--
-- See the various ...attributes functions in the where clause for details.
isDuplicateTransaction :: HL.Journal -> HL.Transaction -> Bool
isDuplicateTransaction  journal trans = any ((==EQ) . cmpTransaction trans) (HL.jtxns journal)
  where
    -- | Transaction attributes that are compared to determine duplicates
    transactionAttributes =
      [ cmp HL.tdate, cmp HL.tdate2, cmp HL.tdescription, cmp HL.tstatus
      , cmp HL.tcode, cmpPostings `on` HL.tpostings
      ]

    -- | Posting attributes that are compared to determine duplicates
    postingAttributes =
      [ cmp HL.pdate, cmp HL.pdate2, cmp HL.pstatus, cmp HL.paccount
      , cmpMixedAmount `on` HL.pamount, cmpPType `on` HL.ptype
      , cmp HL.pbalanceassertion
      ]

    -- | Ammount attributes that are compared to determine duplicates
    amountAttributes =
      [ cmp HL.acommodity, cmp HL.aprice, cmp HL.aquantity ]

    -- | Compare two transactions but ignore unimportant details
    cmpTransaction :: HL.Transaction -> HL.Transaction -> Ordering
    cmpTransaction = lexical transactionAttributes

    
    -- | Compare two posting lists of postings by sorting them deterministically
    -- and then compare correspondings list elements
    cmpPostings :: [HL.Posting] -> [HL.Posting] -> Ordering
    cmpPostings ps1 ps2 =
      mconcat (zipWith (lexical postingAttributes) (sortPostings ps1) (sortPostings ps2))

    -- | Compare two posting styles (this should really be an Eq instance)
    cmpPType :: HL.PostingType -> HL.PostingType -> Ordering
    cmpPType = compare `on` pTypeToInt
      where
        pTypeToInt :: HL.PostingType -> Int
        pTypeToInt HL.RegularPosting = 0
        pTypeToInt HL.VirtualPosting = 1
        pTypeToInt HL.BalancedVirtualPosting = 2

    -- | Compare two amounts ignoring unimportant details
    cmpAmount :: HL.Amount -> HL.Amount -> Ordering
    cmpAmount = lexical amountAttributes

    -- | Compare two mixed amounts by first sorting the individual amounts
    -- deterministically and then comparing them one-by-one.
    cmpMixedAmount :: HL.MixedAmount -> HL.MixedAmount -> Ordering
    cmpMixedAmount (HL.Mixed as1) (HL.Mixed as2) =
      let
        sortedAs1 = sortBy cmpAmount as1
        sortedAs2 = sortBy cmpAmount as2
      in
        mconcat $
          compare (length as1) (length as2) : zipWith cmpAmount sortedAs1 sortedAs2

    sortPostings :: [HL.Posting] -> [HL.Posting]
    sortPostings = sortBy (lexical postingAttributes)

    -- | Shortcut for 'compare `on`'
    cmp :: Ord b => (a -> b) -> a -> a -> Ordering
    cmp f = compare `on` f

    -- | Apply two things with multiple predicats and combine the results lexicographically
    lexical :: [a -> b -> Ordering] -> a -> b -> Ordering
    lexical fs x y = mconcat (map (\f -> f x y) fs)

fromEither :: Either a a -> a
fromEither = either id id
