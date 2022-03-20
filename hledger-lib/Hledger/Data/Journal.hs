{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-|

A 'Journal' is a set of transactions, plus optional related data.  This is
hledger's primary data object. It is usually parsed from a journal file or
other data format (see "Hledger.Read").

-}

module Hledger.Data.Journal (
  -- * Parsing helpers
  JournalParser,
  ErroringJournalParser,
  addPriceDirective,
  addTransactionModifier,
  addPeriodicTransaction,
  addTransaction,
  journalInferMarketPricesFromTransactions,
  journalApplyCommodityStyles,
  commodityStylesFromAmounts,
  journalCommodityStyles,
  journalToCost,
  journalAddInferredEquityPostings,
  journalReverse,
  journalSetLastReadTime,
  journalPivot,
  -- * Filtering
  filterJournalTransactions,
  filterJournalPostings,
  filterJournalRelatedPostings,
  filterJournalAmounts,
  filterTransactionAmounts,
  filterTransactionPostings,
  filterTransactionRelatedPostings,
  filterPostingAmount,
  -- * Mapping
  journalMapTransactions,
  journalMapPostings,
  journalMapPostingAmounts,
  -- * Querying
  journalAccountNamesUsed,
  journalAccountNamesImplied,
  journalAccountNamesDeclared,
  journalAccountNamesDeclaredOrUsed,
  journalAccountNamesDeclaredOrImplied,
  journalLeafAccountNamesDeclared,
  journalAccountNames,
  journalLeafAccountNames,
  journalAccountNameTree,
  journalAccountTags,
  journalInheritedAccountTags,
  -- journalAmountAndPriceCommodities,
  -- journalAmountStyles,
  -- overJournalAmounts,
  -- traverseJournalAmounts,
  -- journalCanonicalCommodities,
  journalPayeesDeclared,
  journalPayeesUsed,
  journalPayeesDeclaredOrUsed,
  journalCommoditiesDeclared,
  journalCommodities,
  journalDateSpan,
  journalDateSpanBothDates,
  journalStartDate,
  journalEndDate,
  journalLastDay,
  journalDescriptions,
  journalFilePath,
  journalFilePaths,
  journalTransactionAt,
  journalNextTransaction,
  journalPrevTransaction,
  journalPostings,
  journalTransactionsSimilarTo,
  -- * Account types
  journalAccountType,
  journalAccountTypes,
  journalAddAccountTypes,
  journalPostingsAddAccountTags,
  -- journalPrices,
  -- * Standard account types
  journalBalanceSheetAccountQuery,
  journalProfitAndLossAccountQuery,
  journalRevenueAccountQuery,
  journalExpenseAccountQuery,
  journalAssetAccountQuery,
  journalLiabilityAccountQuery,
  journalEquityAccountQuery,
  journalCashAccountQuery,
  journalConversionAccount,
  -- * Misc
  canonicalStyleFrom,
  nulljournal,
  journalNumberTransactions,
  journalNumberAndTieTransactions,
  journalUntieTransactions,
  journalModifyTransactions,
  journalApplyAliases,
  -- * Tests
  samplejournal,
  samplejournalMaybeExplicit,
  tests_Journal
  --
)
where

import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT(..))
import Control.Monad.State.Strict (StateT)
import Data.Char (toUpper, isDigit)
import Data.Default (Default(..))
import Data.Foldable (toList)
import Data.List ((\\), find, foldl', sortBy, union)
import Data.List.Extra (nubSort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Safe (headMay, headDef, maximumMay, minimumMay)
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Tree (Tree(..), flatten)
import Text.Printf (printf)
import Text.Megaparsec (ParsecT)
import Text.Megaparsec.Custom (FinalParseError)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.AccountName
import Hledger.Data.Amount
import Hledger.Data.Posting
import Hledger.Data.Transaction
import Hledger.Data.TransactionModifier
import Hledger.Data.Valuation
import Hledger.Query


-- | A parser of text that runs in some monad, keeping a Journal as state.
type JournalParser m a = StateT Journal (ParsecT HledgerParseErrorData Text m) a

-- | A parser of text that runs in some monad, keeping a Journal as
-- state, that can throw an exception to end parsing, preventing
-- further parser backtracking.
type ErroringJournalParser m a =
  StateT Journal (ParsecT HledgerParseErrorData Text (ExceptT FinalParseError m)) a

-- deriving instance Show Journal
instance Show Journal where
  show j
    | debugLevel < 3 = printf "Journal %s with %d transactions, %d accounts"
             (journalFilePath j)
             (length $ jtxns j)
             (length accounts)
    | debugLevel < 6 = printf "Journal %s with %d transactions, %d accounts: %s"
             (journalFilePath j)
             (length $ jtxns j)
             (length accounts)
             (show accounts)
    | otherwise = printf "Journal %s with %d transactions, %d accounts: %s, commodity styles: %s"
             (journalFilePath j)
             (length $ jtxns j)
             (length accounts)
             (show accounts)
             (show $ jinferredcommodities j)
             -- ++ (show $ journalTransactions l)
             where accounts = filter (/= "root") $ flatten $ journalAccountNameTree j

-- showJournalDebug j = unlines [
--                       show j
--                      ,show (jtxns j)
--                      ,show (jtxnmodifiers j)
--                      ,show (jperiodictxns j)
--                      ,show $ jparsetimeclockentries j
--                      ,show $ jpricedirectives j
--                      ,show $ jfinalcommentlines j
--                      ,show $ jparsestate j
--                      ,show $ map fst $ jfiles j
--                      ]

-- The semigroup instance for Journal is useful for two situations.
--
-- 1. concatenating finalised journals, eg with multiple -f options:
-- FIRST <> SECOND. The second's list fields are appended to the
-- first's, map fields are combined, transaction counts are summed,
-- the parse state of the second is kept.
--
-- 2. merging a child parsed journal, eg with the include directive:
-- CHILD <> PARENT. A parsed journal's data is in reverse order, so
-- this gives what we want.
--
-- Note that (<>) is right-biased, so nulljournal is only a left identity.
-- In particular, this prevents Journal from being a monoid.
instance Semigroup Journal where
  j1 <> j2 = Journal {
     jparsedefaultyear          = jparsedefaultyear          j2
    ,jparsedefaultcommodity     = jparsedefaultcommodity     j2
    ,jparsedecimalmark          = jparsedecimalmark          j2
    ,jparseparentaccounts       = jparseparentaccounts       j2
    ,jparsealiases              = jparsealiases              j2
    -- ,jparsetransactioncount     = jparsetransactioncount     j1 +  jparsetransactioncount     j2
    ,jparsetimeclockentries     = jparsetimeclockentries     j1 <> jparsetimeclockentries     j2
    ,jincludefilestack          = jincludefilestack j2
    ,jdeclaredpayees            = jdeclaredpayees            j1 <> jdeclaredpayees            j2
    ,jdeclaredaccounts          = jdeclaredaccounts          j1 <> jdeclaredaccounts          j2
    ,jdeclaredaccounttags       = jdeclaredaccounttags       j1 <> jdeclaredaccounttags       j2
    ,jdeclaredaccounttypes      = jdeclaredaccounttypes      j1 <> jdeclaredaccounttypes      j2
    ,jaccounttypes              = jaccounttypes              j1 <> jaccounttypes              j2
    ,jglobalcommoditystyles     = jglobalcommoditystyles     j1 <> jglobalcommoditystyles     j2
    ,jcommodities               = jcommodities               j1 <> jcommodities               j2
    ,jinferredcommodities       = jinferredcommodities       j1 <> jinferredcommodities       j2
    ,jpricedirectives           = jpricedirectives           j1 <> jpricedirectives           j2
    ,jinferredmarketprices      = jinferredmarketprices      j1 <> jinferredmarketprices      j2
    ,jtxnmodifiers              = jtxnmodifiers              j1 <> jtxnmodifiers              j2
    ,jperiodictxns              = jperiodictxns              j1 <> jperiodictxns              j2
    ,jtxns                      = jtxns                      j1 <> jtxns                      j2
    ,jfinalcommentlines         = jfinalcommentlines j2  -- XXX discards j1's ?
    ,jfiles                     = jfiles                     j1 <> jfiles                     j2
    ,jlastreadtime              = max (jlastreadtime j1) (jlastreadtime j2)
    }

instance Default Journal where
  def = nulljournal

nulljournal :: Journal
nulljournal = Journal {
   jparsedefaultyear          = Nothing
  ,jparsedefaultcommodity     = Nothing
  ,jparsedecimalmark          = Nothing
  ,jparseparentaccounts       = []
  ,jparsealiases              = []
  -- ,jparsetransactioncount     = 0
  ,jparsetimeclockentries     = []
  ,jincludefilestack          = []
  ,jdeclaredpayees            = []
  ,jdeclaredaccounts          = []
  ,jdeclaredaccounttags       = M.empty
  ,jdeclaredaccounttypes      = M.empty
  ,jaccounttypes              = M.empty
  ,jglobalcommoditystyles     = M.empty
  ,jcommodities               = M.empty
  ,jinferredcommodities       = M.empty
  ,jpricedirectives           = []
  ,jinferredmarketprices      = []
  ,jtxnmodifiers              = []
  ,jperiodictxns              = []
  ,jtxns                      = []
  ,jfinalcommentlines         = ""
  ,jfiles                     = []
  ,jlastreadtime              = 0
  }

journalFilePath :: Journal -> FilePath
journalFilePath = fst . mainfile

journalFilePaths :: Journal -> [FilePath]
journalFilePaths = map fst . jfiles

mainfile :: Journal -> (FilePath, Text)
mainfile = headDef ("", "") . jfiles

addTransaction :: Transaction -> Journal -> Journal
addTransaction t j = j { jtxns = t : jtxns j }

addTransactionModifier :: TransactionModifier -> Journal -> Journal
addTransactionModifier mt j = j { jtxnmodifiers = mt : jtxnmodifiers j }

addPeriodicTransaction :: PeriodicTransaction -> Journal -> Journal
addPeriodicTransaction pt j = j { jperiodictxns = pt : jperiodictxns j }

addPriceDirective :: PriceDirective -> Journal -> Journal
addPriceDirective h j = j { jpricedirectives = h : jpricedirectives j }  -- XXX #999 keep sorted

-- | Get the transaction with this index (its 1-based position in the input stream), if any.
journalTransactionAt :: Journal -> Integer -> Maybe Transaction
journalTransactionAt Journal{jtxns=ts} i =
  -- it's probably ts !! (i+1), but we won't assume
  headMay [t | t <- ts, tindex t == i]

-- | Get the transaction that appeared immediately after this one in the input stream, if any.
journalNextTransaction :: Journal -> Transaction -> Maybe Transaction
journalNextTransaction j t = journalTransactionAt j (tindex t + 1)

-- | Get the transaction that appeared immediately before this one in the input stream, if any.
journalPrevTransaction :: Journal -> Transaction -> Maybe Transaction
journalPrevTransaction j t = journalTransactionAt j (tindex t - 1)

-- | All postings from this journal's transactions, in order.
journalPostings :: Journal -> [Posting]
journalPostings = concatMap tpostings . jtxns

-- | Sorted unique commodity symbols declared by commodity directives in this journal.
journalCommoditiesDeclared :: Journal -> [CommoditySymbol]
journalCommoditiesDeclared = M.keys . jcommodities

-- | Sorted unique commodity symbols declared or inferred from this journal.
journalCommodities :: Journal -> S.Set CommoditySymbol
journalCommodities j = M.keysSet (jcommodities j) <> M.keysSet (jinferredcommodities j)

-- | Unique transaction descriptions used in this journal.
journalDescriptions :: Journal -> [Text]
journalDescriptions = nubSort . map tdescription . jtxns

-- | Sorted unique payees declared by payee directives in this journal.
journalPayeesDeclared :: Journal -> [Payee]
journalPayeesDeclared = nubSort . map fst . jdeclaredpayees

-- | Sorted unique payees used by transactions in this journal.
journalPayeesUsed :: Journal -> [Payee]
journalPayeesUsed = nubSort . map transactionPayee . jtxns

-- | Sorted unique payees used in transactions or declared by payee directives in this journal.
journalPayeesDeclaredOrUsed :: Journal -> [Payee]
journalPayeesDeclaredOrUsed j = toList $ foldMap S.fromList
    [journalPayeesDeclared j, journalPayeesUsed j]

-- | Sorted unique account names posted to by this journal's transactions.
journalAccountNamesUsed :: Journal -> [AccountName]
journalAccountNamesUsed = accountNamesFromPostings . journalPostings

-- | Sorted unique account names implied by this journal's transactions -
-- accounts posted to and all their implied parent accounts.
journalAccountNamesImplied :: Journal -> [AccountName]
journalAccountNamesImplied = expandAccountNames . journalAccountNamesUsed

-- | Sorted unique account names declared by account directives in this journal.
journalAccountNamesDeclared :: Journal -> [AccountName]
journalAccountNamesDeclared = nubSort . map fst . jdeclaredaccounts

-- | Sorted unique account names declared by account directives in this journal,
-- which have no children.
journalLeafAccountNamesDeclared :: Journal -> [AccountName]
journalLeafAccountNamesDeclared = treeLeaves . accountNameTreeFrom . journalAccountNamesDeclared

-- | Sorted unique account names declared by account directives or posted to
-- by transactions in this journal.
journalAccountNamesDeclaredOrUsed :: Journal -> [AccountName]
journalAccountNamesDeclaredOrUsed j = toList $ foldMap S.fromList
    [journalAccountNamesDeclared j, journalAccountNamesUsed j]

-- | Sorted unique account names declared by account directives, or posted to
-- or implied as parents by transactions in this journal.
journalAccountNamesDeclaredOrImplied :: Journal -> [AccountName]
journalAccountNamesDeclaredOrImplied j = toList $ foldMap S.fromList
    [journalAccountNamesDeclared j, expandAccountNames $ journalAccountNamesUsed j]

-- | Convenience/compatibility alias for journalAccountNamesDeclaredOrImplied.
journalAccountNames :: Journal -> [AccountName]
journalAccountNames = journalAccountNamesDeclaredOrImplied

-- | Sorted unique account names declared or implied in this journal
-- which have no children.
journalLeafAccountNames :: Journal -> [AccountName]
journalLeafAccountNames = treeLeaves . journalAccountNameTree

journalAccountNameTree :: Journal -> Tree AccountName
journalAccountNameTree = accountNameTreeFrom . journalAccountNamesDeclaredOrImplied

-- | Which tags have been declared explicitly for this account, if any ?
journalAccountTags :: Journal -> AccountName -> [Tag]
journalAccountTags Journal{jdeclaredaccounttags} a = M.findWithDefault [] a jdeclaredaccounttags

-- | Which tags are in effect for this account, including tags inherited from parent accounts ?
journalInheritedAccountTags :: Journal -> AccountName -> [Tag]
journalInheritedAccountTags j a =
  foldl' (\ts a -> ts `union` journalAccountTags j a) [] as
  where
    as = a : parentAccountNames a
-- PERF: cache in journal ?

-- | Find up to N most similar and most recent transactions matching
-- the given transaction description and query. Transactions are
-- listed with their description's similarity score (see
-- compareDescriptions), sorted by highest score and then by date.
-- Only transactions with a similarity score greater than a minimum
-- threshold (currently 0) are returned.
journalTransactionsSimilarTo :: Journal -> Query -> Text -> Int -> [(Double,Transaction)]
journalTransactionsSimilarTo Journal{jtxns} q desc n =
  take n $
  sortBy (\(s1,t1) (s2,t2) -> compare (s2,tdate t2) (s1,tdate t1)) $
  filter ((> threshold).fst)
  [(compareDescriptions desc $ tdescription t, t) | t <- jtxns, q `matchesTransaction` t]
  where
    threshold = 0

-- | Return a similarity score from 0 to 1.5 for two transaction descriptions. 
-- This is based on compareStrings, with the following modifications:
--
-- - numbers are stripped out before measuring similarity
--
-- - if the (unstripped) first description appears in its entirety within the second,
--   the score is boosted by 0.5.
--
compareDescriptions :: Text -> Text -> Double
compareDescriptions a b =
  (if a `T.isInfixOf` b then (0.5+) else id) $
  compareStrings (simplify a) (simplify b)
  where
    simplify = T.unpack . T.filter (not.isDigit)

-- | Return a similarity score from 0 to 1 for two strings.  This
-- was based on Simon White's string similarity algorithm
-- (http://www.catalysoft.com/articles/StrikeAMatch.html), later found
-- to be https://en.wikipedia.org/wiki/S%C3%B8rensen%E2%80%93Dice_coefficient,
-- and modified to handle short strings better.
-- Todo: check out http://nlp.fi.muni.cz/raslan/2008/raslan08.pdf#page=14 .
compareStrings :: String -> String -> Double
compareStrings "" "" = 1
compareStrings [_] "" = 0
compareStrings "" [_] = 0
compareStrings [a] [b] = if toUpper a == toUpper b then 1 else 0
compareStrings s1 s2 = 2 * commonpairs / totalpairs
  where
    pairs1      = S.fromList $ wordLetterPairs $ uppercase s1
    pairs2      = S.fromList $ wordLetterPairs $ uppercase s2
    commonpairs = fromIntegral $ S.size $ S.intersection pairs1 pairs2
    totalpairs  = fromIntegral $ S.size pairs1 + S.size pairs2

wordLetterPairs :: String -> [String]
wordLetterPairs = concatMap letterPairs . words

letterPairs :: String -> [String]
letterPairs (a:b:rest) = [a,b] : letterPairs (b:rest)
letterPairs _ = []

-- Older account type code

-- queries for standard account types

-- | Get a query for accounts of the specified types in this journal. 
-- Account types include:
-- Asset, Liability, Equity, Revenue, Expense, Cash, Conversion.
-- For each type, if no accounts were declared with this type, the query 
-- will instead match accounts with names matched by the case-insensitive 
-- regular expression provided as a fallback.
-- The query will match all accounts which were declared as one of
-- these types (by account directives with the type: tag), plus all their 
-- subaccounts which have not been declared as some other type.
--
-- This is older code pre-dating 2022's expansion of account types.
journalAccountTypeQuery :: [AccountType] -> Regexp -> Journal -> Query
journalAccountTypeQuery atypes fallbackregex Journal{jdeclaredaccounttypes} =
  let
    declaredacctsoftype :: [AccountName] =
      concat $ mapMaybe (`M.lookup` jdeclaredaccounttypes) atypes
  in case declaredacctsoftype of
    [] -> Acct fallbackregex
    as -> And $ Or acctnameRegexes : if null differentlyTypedRegexes then [] else [ Not $ Or differentlyTypedRegexes ]
      where
        -- XXX Query isn't able to match account type since that requires extra info from the journal.
        -- So we do a hacky search by name instead.
        acctnameRegexes = map (Acct . accountNameToAccountRegex) as
        differentlyTypedRegexes = map (Acct . accountNameToAccountRegex) differentlytypedsubs

        differentlytypedsubs = concat
          [subs | (t,bs) <- M.toList jdeclaredaccounttypes
              , t `notElem` atypes
              , let subs = [b | b <- bs, any (`isAccountNamePrefixOf` b) as]
          ]

-- | A query for accounts in this journal which have been
-- declared as Asset (or Cash, a subtype of Asset) by account directives, 
-- or otherwise for accounts with names matched by the case-insensitive 
-- regular expression @^assets?(:|$)@.
journalAssetAccountQuery :: Journal -> Query
journalAssetAccountQuery j =
  Or [
     journalAccountTypeQuery [Asset] assetAccountRegex j
    ,journalCashAccountOnlyQuery j
  ]

-- | A query for Cash (liquid asset) accounts in this journal, ie accounts
-- declared as Cash by account directives, or otherwise accounts whose
-- names match the case-insensitive regular expression
-- @(^assets:(.+:)?(cash|bank)(:|$)@.
journalCashAccountQuery :: Journal -> Query
journalCashAccountQuery = journalAccountTypeQuery [Cash] cashAccountRegex

-- | A query for accounts in this journal specifically declared as Cash by 
-- account directives, or otherwise the None query.
journalCashAccountOnlyQuery :: Journal -> Query
journalCashAccountOnlyQuery j
  -- Cash accounts are declared; get a query for them (the fallback regex won't be used)
  | Cash `M.member` jdeclaredaccounttypes j = journalAccountTypeQuery [Cash] notused j
  | otherwise = None
  where notused = error' "journalCashAccountOnlyQuery: this should not have happened!"  -- PARTIAL:

-- | A query for accounts in this journal which have been
-- declared as Liability by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression
-- @^(debts?|liabilit(y|ies))(:|$)@.
journalLiabilityAccountQuery :: Journal -> Query
journalLiabilityAccountQuery = journalAccountTypeQuery [Liability] liabilityAccountRegex

-- | A query for accounts in this journal which have been
-- declared as Equity by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression
-- @^equity(:|$)@.
journalEquityAccountQuery :: Journal -> Query
journalEquityAccountQuery = journalAccountTypeQuery [Equity] equityAccountRegex

-- | A query for accounts in this journal which have been
-- declared as Revenue by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression
-- @^(income|revenue)s?(:|$)@.
journalRevenueAccountQuery :: Journal -> Query
journalRevenueAccountQuery = journalAccountTypeQuery [Revenue] revenueAccountRegex

-- | A query for accounts in this journal which have been
-- declared as Expense by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression
-- @^expenses?(:|$)@.
journalExpenseAccountQuery  :: Journal -> Query
journalExpenseAccountQuery = journalAccountTypeQuery [Expense] expenseAccountRegex

-- | A query for Asset, Liability & Equity accounts in this journal.
-- Cf <http://en.wikipedia.org/wiki/Chart_of_accounts#Balance_Sheet_Accounts>.
journalBalanceSheetAccountQuery :: Journal -> Query
journalBalanceSheetAccountQuery j = Or [journalAssetAccountQuery j
                                       ,journalLiabilityAccountQuery j
                                       ,journalEquityAccountQuery j
                                       ]

-- | A query for Profit & Loss accounts in this journal.
-- Cf <http://en.wikipedia.org/wiki/Chart_of_accounts#Profit_.26_Loss_accounts>.
journalProfitAndLossAccountQuery  :: Journal -> Query
journalProfitAndLossAccountQuery j = Or [journalRevenueAccountQuery j
                                        ,journalExpenseAccountQuery j
                                        ]

-- | The 'AccountName' to use for automatically generated conversion postings.
journalConversionAccount :: Journal -> AccountName
journalConversionAccount =
    headDef (T.pack "equity:conversion")
    . M.findWithDefault [] Conversion
    . jdeclaredaccounttypes

-- Newer account type code.

journalAccountType :: Journal -> AccountName -> Maybe AccountType
journalAccountType Journal{jaccounttypes} = accountNameType jaccounttypes

-- | Add a map of all known account types to the journal.
journalAddAccountTypes :: Journal -> Journal
journalAddAccountTypes j = j{jaccounttypes = journalAccountTypes j}

-- | Build a map of all known account types, explicitly declared
-- or inferred from the account's parent or name.
journalAccountTypes :: Journal -> M.Map AccountName AccountType
journalAccountTypes j = M.fromList [(a,acctType) | (a, Just (acctType,_)) <- flatten t']
  where
    t = accountNameTreeFrom $ journalAccountNames j :: Tree AccountName
    -- Map from the top of the account tree down to the leaves, propagating
    -- account types downward. Keep track of whether the account is declared
    -- (True), in which case the parent account should be preferred, or merely
    -- inferred (False), in which case the inferred type should be preferred.
    t' = settypes Nothing t :: Tree (AccountName, Maybe (AccountType, Bool))
      where
        settypes :: Maybe (AccountType, Bool) -> Tree AccountName -> Tree (AccountName, Maybe (AccountType, Bool))
        settypes mparenttype (Node a subs) = Node (a, mtype) (map (settypes mtype) subs)
          where
            mtype = M.lookup a declaredtypes <|> minferred
              where 
                declaredtypes = (,True) <$> journalDeclaredAccountTypes j
                minferred = if maybe False snd mparenttype
                            then mparenttype
                            else (,False) <$> accountNameInferType a <|> mparenttype

-- | Build a map of the account types explicitly declared.
journalDeclaredAccountTypes :: Journal -> M.Map AccountName AccountType
journalDeclaredAccountTypes Journal{jdeclaredaccounttypes} =
  M.fromList $ concat [map (,t) as | (t,as) <- M.toList jdeclaredaccounttypes]

-- | To all postings in the journal, add any tags from their account
-- (including those inherited from parent accounts).
-- If the same tag exists on posting and account, the latter is ignored.
journalPostingsAddAccountTags :: Journal -> Journal
journalPostingsAddAccountTags j = journalMapPostings addtags j
  where addtags p = p `postingAddTags` (journalInheritedAccountTags j $ paccount p)

-- Various kinds of filtering on journals. We do it differently depending
-- on the command.

-------------------------------------------------------------------------------
-- filtering V2

-- | Keep only transactions matching the query expression.
filterJournalTransactions :: Query -> Journal -> Journal
filterJournalTransactions q j@Journal{jtxns} = j{jtxns=filter (matchesTransactionExtra (journalAccountType j) q) jtxns}

-- | Keep only postings matching the query expression.
-- This can leave unbalanced transactions.
filterJournalPostings :: Query -> Journal -> Journal
filterJournalPostings q j@Journal{jtxns=ts} = j{jtxns=map (filterTransactionPostingsExtra (journalAccountType j) q) ts}

-- | Keep only postings which do not match the query expression, but for which a related posting does.
-- This can leave unbalanced transactions.
filterJournalRelatedPostings :: Query -> Journal -> Journal
filterJournalRelatedPostings q j@Journal{jtxns=ts} = j{jtxns=map (filterTransactionRelatedPostings q) ts}

-- | Within each posting's amount, keep only the parts matching the query, and
-- remove any postings with all amounts removed.
-- This can leave unbalanced transactions.
filterJournalAmounts :: Query -> Journal -> Journal
filterJournalAmounts q j@Journal{jtxns=ts} = j{jtxns=map (filterTransactionAmounts q) ts}

-- | Filter out all parts of this transaction's amounts which do not match the
-- query, and remove any postings with all amounts removed.
-- This can leave the transaction unbalanced.
filterTransactionAmounts :: Query -> Transaction -> Transaction
filterTransactionAmounts q t@Transaction{tpostings=ps} = t{tpostings=mapMaybe (filterPostingAmount q) ps}

-- | Filter out all parts of this posting's amount which do not match the query, and remove the posting
-- if this removes all amounts.
filterPostingAmount :: Query -> Posting -> Maybe Posting
filterPostingAmount q p@Posting{pamount=as}
  | null newamt = Nothing
  | otherwise   = Just p{pamount=Mixed newamt}
  where
    Mixed newamt = filterMixedAmount (q `matchesAmount`) as

filterTransactionPostings :: Query -> Transaction -> Transaction
filterTransactionPostings q t@Transaction{tpostings=ps} = t{tpostings=filter (q `matchesPosting`) ps}

-- Like filterTransactionPostings, but is given the map of account types so can also filter by account type.
filterTransactionPostingsExtra :: (AccountName -> Maybe AccountType) -> Query -> Transaction -> Transaction
filterTransactionPostingsExtra atypes q t@Transaction{tpostings=ps} =
  t{tpostings=filter (matchesPostingExtra atypes q) ps}

filterTransactionRelatedPostings :: Query -> Transaction -> Transaction
filterTransactionRelatedPostings q t@Transaction{tpostings=ps} =
    t{tpostings=if null matches then [] else ps \\ matches}
  where matches = filter (matchesPosting q) ps

-- | Apply a transformation to a journal's transactions.
journalMapTransactions :: (Transaction -> Transaction) -> Journal -> Journal
journalMapTransactions f j@Journal{jtxns=ts} = j{jtxns=map f ts}

-- | Apply a transformation to a journal's postings.
journalMapPostings :: (Posting -> Posting) -> Journal -> Journal
journalMapPostings f j@Journal{jtxns=ts} = j{jtxns=map (transactionMapPostings f) ts}

-- | Apply a transformation to a journal's posting amounts.
journalMapPostingAmounts :: (MixedAmount -> MixedAmount) -> Journal -> Journal
journalMapPostingAmounts f = journalMapPostings (postingTransformAmount f)

{-
-------------------------------------------------------------------------------
-- filtering V1

-- | Keep only transactions we are interested in, as described by the
-- filter specification.
filterJournalTransactions :: FilterSpec -> Journal -> Journal
filterJournalTransactions FilterSpec{datespan=datespan
                                    ,cleared=cleared
                                    -- ,real=real
                                    -- ,empty=empty
                                    ,acctpats=apats
                                    ,descpats=dpats
                                    ,depth=depth
                                    ,fMetadata=md
                                    } =
    filterJournalTransactionsByStatus cleared .
    filterJournalPostingsByDepth depth .
    filterJournalTransactionsByAccount apats .
    filterJournalTransactionsByMetadata md .
    filterJournalTransactionsByDescription dpats .
    filterJournalTransactionsByDate datespan

-- | Keep only postings we are interested in, as described by the filter
-- specification. This can leave unbalanced transactions.
filterJournalPostings :: FilterSpec -> Journal -> Journal
filterJournalPostings FilterSpec{datespan=datespan
                                ,cleared=cleared
                                ,real=real
                                ,empty=empty
                                ,acctpats=apats
                                ,descpats=dpats
                                ,depth=depth
                                ,fMetadata=md
                                } =
    filterJournalPostingsByRealness real .
    filterJournalPostingsByStatus cleared .
    filterJournalPostingsByEmpty empty .
    filterJournalPostingsByDepth depth .
    filterJournalPostingsByAccount apats .
    filterJournalTransactionsByMetadata md .
    filterJournalTransactionsByDescription dpats .
    filterJournalTransactionsByDate datespan

-- | Keep only transactions whose metadata matches all metadata specifications.
filterJournalTransactionsByMetadata :: [(String,String)] -> Journal -> Journal
filterJournalTransactionsByMetadata pats j@Journal{jtxns=ts} = j{jtxns=filter matchmd ts}
    where matchmd t = all (`elem` tmetadata t) pats

-- | Keep only transactions whose description matches the description patterns.
filterJournalTransactionsByDescription :: [String] -> Journal -> Journal
filterJournalTransactionsByDescription pats j@Journal{jtxns=ts} = j{jtxns=filter matchdesc ts}
    where matchdesc = matchpats pats . tdescription

-- | Keep only transactions which fall between begin and end dates.
-- We include transactions on the begin date and exclude transactions on the end
-- date, like ledger.  An empty date string means no restriction.
filterJournalTransactionsByDate :: DateSpan -> Journal -> Journal
filterJournalTransactionsByDate (DateSpan begin end) j@Journal{jtxns=ts} = j{jtxns=filter match ts}
    where match t = maybe True (tdate t>=) begin && maybe True (tdate t<) end

-- | Keep only transactions which have the requested cleared/uncleared
-- status, if there is one.
filterJournalTransactionsByStatus :: Maybe Bool -> Journal -> Journal
filterJournalTransactionsByStatus Nothing j = j
filterJournalTransactionsByStatus (Just val) j@Journal{jtxns=ts} = j{jtxns=filter match ts}
    where match = (==val).tstatus

-- | Keep only postings which have the requested cleared/uncleared status,
-- if there is one.
filterJournalPostingsByStatus :: Maybe Bool -> Journal -> Journal
filterJournalPostingsByStatus Nothing j = j
filterJournalPostingsByStatus (Just c) j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
    where filterpostings t@Transaction{tpostings=ps} = t{tpostings=filter ((==c) . postingCleared) ps}

-- | Strip out any virtual postings, if the flag is true, otherwise do
-- no filtering.
filterJournalPostingsByRealness :: Bool -> Journal -> Journal
filterJournalPostingsByRealness False j = j
filterJournalPostingsByRealness True j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
    where filterpostings t@Transaction{tpostings=ps} = t{tpostings=filter isReal ps}

-- | Strip out any postings with zero amount, unless the flag is true.
filterJournalPostingsByEmpty :: Bool -> Journal -> Journal
filterJournalPostingsByEmpty True j = j
filterJournalPostingsByEmpty False j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
    where filterpostings t@Transaction{tpostings=ps} = t{tpostings=filter (not . isEmptyPosting) ps}

-- -- | Keep only transactions which affect accounts deeper than the specified depth.
-- filterJournalTransactionsByDepth :: Maybe Int -> Journal -> Journal
-- filterJournalTransactionsByDepth Nothing j = j
-- filterJournalTransactionsByDepth (Just d) j@Journal{jtxns=ts} =
--     j{jtxns=(filter (any ((<= d+1) . accountNameLevel . paccount) . tpostings) ts)}

-- | Strip out any postings to accounts deeper than the specified depth
-- (and any transactions which have no postings as a result).
filterJournalPostingsByDepth :: Maybe Int -> Journal -> Journal
filterJournalPostingsByDepth Nothing j = j
filterJournalPostingsByDepth (Just d) j@Journal{jtxns=ts} =
    j{jtxns=filter (not . null . tpostings) $ map filtertxns ts}
    where filtertxns t@Transaction{tpostings=ps} =
              t{tpostings=filter ((<= d) . accountNameLevel . paccount) ps}

-- | Keep only postings which affect accounts matched by the account patterns.
-- This can leave transactions unbalanced.
filterJournalPostingsByAccount :: [String] -> Journal -> Journal
filterJournalPostingsByAccount apats j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
    where filterpostings t@Transaction{tpostings=ps} = t{tpostings=filter (matchpats apats . paccount) ps}

-- | Keep only transactions which affect accounts matched by the account patterns.
-- More precisely: each positive account pattern excludes transactions
-- which do not contain a posting to a matched account, and each negative
-- account pattern excludes transactions containing a posting to a matched
-- account.
filterJournalTransactionsByAccount :: [String] -> Journal -> Journal
filterJournalTransactionsByAccount apats j@Journal{jtxns=ts} = j{jtxns=filter tmatch ts}
    where
      tmatch t = (null positives || any positivepmatch ps) && (null negatives || not (any negativepmatch ps)) where ps = tpostings t
      positivepmatch p = any (`amatch` a) positives where a = paccount p
      negativepmatch p = any (`amatch` a) negatives where a = paccount p
      amatch pat a = regexMatchesCI (abspat pat) a
      (negatives,positives) = partition isnegativepat apats

-}

-- | Reverse all lists of parsed items, which during parsing were
-- prepended to, so that the items are in parse order. Part of
-- post-parse finalisation.
journalReverse :: Journal -> Journal
journalReverse j =
  j {jfiles            = reverse $ jfiles j
    ,jdeclaredaccounts = reverse $ jdeclaredaccounts j
    ,jtxns             = reverse $ jtxns j
    ,jtxnmodifiers     = reverse $ jtxnmodifiers j
    ,jperiodictxns     = reverse $ jperiodictxns j
    ,jpricedirectives  = reverse $ jpricedirectives j
    }

-- | Set this journal's last read time, ie when its files were last read.
journalSetLastReadTime :: POSIXTime -> Journal -> Journal
journalSetLastReadTime t j = j{ jlastreadtime = t }


journalNumberAndTieTransactions = journalTieTransactions . journalNumberTransactions

-- | Number (set the tindex field) this journal's transactions, counting upward from 1.
journalNumberTransactions :: Journal -> Journal
journalNumberTransactions j@Journal{jtxns=ts} = j{jtxns=zipWith (\i t -> t{tindex=i}) [1..] ts}

-- | Tie the knot in all of this journal's transactions, ensuring their postings
-- refer to them. This should be done last, after any other transaction-modifying operations.
journalTieTransactions :: Journal -> Journal
journalTieTransactions j@Journal{jtxns=ts} = j{jtxns=map txnTieKnot ts}

-- | Untie all transaction-posting knots in this journal, so that eg
-- recursiveSize and GHCI's :sprint can work on it.
journalUntieTransactions :: Transaction -> Transaction
journalUntieTransactions t@Transaction{tpostings=ps} = t{tpostings=map (\p -> p{ptransaction=Nothing}) ps}

-- | Apply any transaction modifier rules in the journal (adding automated
-- postings to transactions, eg). Or if a modifier rule fails to parse,
-- return the error message. A reference date is provided to help interpret
-- relative dates in transaction modifier queries.
journalModifyTransactions :: Day -> Journal -> Either String Journal
journalModifyTransactions d j =
    case modifyTransactions (journalAccountType j) (journalInheritedAccountTags j) (journalCommodityStyles j) d (jtxnmodifiers j) (jtxns j) of
      Right ts -> Right j{jtxns=ts}
      Left err -> Left err

--

-- | Choose and apply a consistent display style to the posting
-- amounts in each commodity (see journalCommodityStyles).
-- Can return an error message eg if inconsistent number formats are found.
journalApplyCommodityStyles :: Journal -> Either String Journal
journalApplyCommodityStyles = fmap fixjournal . journalInferCommodityStyles
  where
    fixjournal j@Journal{jpricedirectives=pds} =
        journalMapPostings (postingApplyCommodityStyles styles) j{jpricedirectives=map fixpricedirective pds}
      where
        styles = journalCommodityStyles j
        fixpricedirective pd@PriceDirective{pdamount=a} = pd{pdamount=styleAmountExceptPrecision styles a}

-- | Get the canonical amount styles for this journal, whether (in order of precedence):
-- set globally in InputOpts,
-- declared by commodity directives, 
-- declared by a default commodity (D) directive, 
-- or inferred from posting amounts, 
-- as a map from symbol to style. 
-- Styles from directives are assumed to specify the decimal mark.
journalCommodityStyles :: Journal -> M.Map CommoditySymbol AmountStyle
journalCommodityStyles j =
  -- XXX could be some redundancy here, cf journalStyleInfluencingAmounts
  globalstyles <> declaredstyles <> defaultcommoditystyle <> inferredstyles
  where
    globalstyles          = jglobalcommoditystyles j
    declaredstyles        = M.mapMaybe cformat $ jcommodities j
    defaultcommoditystyle = M.fromList $ catMaybes [jparsedefaultcommodity j]
    inferredstyles        = jinferredcommodities j

-- | Collect and save inferred amount styles for each commodity based on
-- the posting amounts in that commodity (excluding price amounts), ie:
-- "the format of the first amount, adjusted to the highest precision of all amounts".
-- Can return an error message eg if inconsistent number formats are found.
journalInferCommodityStyles :: Journal -> Either String Journal
journalInferCommodityStyles j =
  case commodityStylesFromAmounts $ journalStyleInfluencingAmounts j of
    Left e   -> Left e
    Right cs -> Right j{jinferredcommodities = dbg7 "journalInferCommodityStyles" cs}

-- | Given a list of amounts, in parse order (roughly speaking; see journalStyleInfluencingAmounts),
-- build a map from their commodity names to standard commodity
-- display formats. Can return an error message eg if inconsistent
-- number formats are found.
--
-- Though, these amounts may have come from multiple files, so we
-- shouldn't assume they use consistent number formats.
-- Currently we don't enforce that even within a single file,
-- and this function never reports an error.
--
commodityStylesFromAmounts :: [Amount] -> Either String (M.Map CommoditySymbol AmountStyle)
commodityStylesFromAmounts =
    Right . foldr (\a -> M.insertWith canonicalStyle (acommodity a) (astyle a)) mempty

-- | Given a list of amount styles (assumed to be from parsed amounts
-- in a single commodity), in parse order, choose a canonical style.
canonicalStyleFrom :: [AmountStyle] -> AmountStyle
canonicalStyleFrom = foldl' canonicalStyle amountstyle

-- TODO: should probably detect and report inconsistencies here.
-- Though, we don't have the info for a good error message, so maybe elsewhere.
-- | Given a pair of AmountStyles, choose a canonical style.
-- This is:
-- the general style of the first amount,
-- with the first digit group style seen,
-- with the maximum precision of all.
canonicalStyle :: AmountStyle -> AmountStyle -> AmountStyle
canonicalStyle a b = a{asprecision=prec, asdecimalpoint=decmark, asdigitgroups=mgrps}
  where
    -- precision is maximum of all precisions
    prec = max (asprecision a) (asprecision b)
    -- identify the digit group mark (& group sizes)
    mgrps = asdigitgroups a <|> asdigitgroups b
    -- if a digit group mark was identified above, we can rely on that;
    -- make sure the decimal mark is different. If not, default to period.
    defdecmark = case mgrps of
        Just (DigitGroups '.' _) -> ','
        _                        -> '.'
    -- identify the decimal mark: the first one used, or the above default,
    -- but never the same character as the digit group mark.
    -- urgh.. refactor..
    decmark = case mgrps of
        Just _  -> Just defdecmark
        Nothing -> asdecimalpoint a <|> asdecimalpoint b <|> Just defdecmark

-- -- | Apply this journal's historical price records to unpriced amounts where possible.
-- journalApplyPriceDirectives :: Journal -> Journal
-- journalApplyPriceDirectives j@Journal{jtxns=ts} = j{jtxns=map fixtransaction ts}
--     where
--       fixtransaction t@Transaction{tdate=d, tpostings=ps} = t{tpostings=map fixposting ps}
--        where
--         fixposting p@Posting{pamount=a} = p{pamount=fixmixedamount a}
--         fixmixedamount = mapMixedAmount fixamount
--         fixamount = fixprice
--         fixprice a@Amount{price=Just _} = a
--         fixprice a@Amount{commodity=c} = a{price=maybe Nothing (Just . UnitPrice) $ journalPriceDirectiveFor j d c}

-- -- | Get the price for a commodity on the specified day from the price database, if known.
-- -- Does only one lookup step, ie will not look up the price of a price.
-- journalPriceDirectiveFor :: Journal -> Day -> CommoditySymbol -> Maybe MixedAmount
-- journalPriceDirectiveFor j d CommoditySymbol{symbol=s} = do
--   let ps = reverse $ filter ((<= d).pddate) $ filter ((s==).hsymbol) $ sortBy (comparing pddate) $ jpricedirectives j
--   case ps of (PriceDirective{pdamount=a}:_) -> Just a
--              _ -> Nothing

-- | Infer transaction-implied market prices from commodity-exchanging
-- transactions, if any. It's best to call this after transactions have
-- been balanced and posting amounts have appropriate prices attached.
journalInferMarketPricesFromTransactions :: Journal -> Journal
journalInferMarketPricesFromTransactions j =
  j{jinferredmarketprices =
       dbg4 "jinferredmarketprices" .
       map priceDirectiveToMarketPrice .
       concatMap postingPriceDirectivesFromCost $
       journalPostings j
   }

-- | Convert all this journal's amounts to cost using the transaction prices, if any.
-- The journal's commodity styles are applied to the resulting amounts.
journalToCost :: ConversionOp -> Journal -> Journal
journalToCost cost j@Journal{jtxns=ts} = j{jtxns=map (transactionToCost styles cost) ts}
  where
    styles = journalCommodityStyles j

-- | Add inferred equity postings to a 'Journal' using transaction prices.
journalAddInferredEquityPostings :: Journal -> Journal
journalAddInferredEquityPostings j = journalMapTransactions (transactionAddInferredEquityPostings equityAcct) j
  where
    equityAcct = journalConversionAccount j

-- -- | Get this journal's unique, display-preference-canonicalised commodities, by symbol.
-- journalCanonicalCommodities :: Journal -> M.Map String CommoditySymbol
-- journalCanonicalCommodities j = canonicaliseCommodities $ journalAmountCommodities j

-- -- | Get all this journal's amounts' commodities, in the order parsed.
-- journalAmountCommodities :: Journal -> [CommoditySymbol]
-- journalAmountCommodities = map acommodity . concatMap amounts . journalAmounts

-- -- | Get all this journal's amount and price commodities, in the order parsed.
-- journalAmountAndPriceCommodities :: Journal -> [CommoditySymbol]
-- journalAmountAndPriceCommodities = concatMap amountCommodities . concatMap amounts . journalAmounts

-- -- | Get this amount's commodity and any commodities referenced in its price.
-- amountCommodities :: Amount -> [CommoditySymbol]
-- amountCommodities Amount{acommodity=c,aprice=p} =
--     case p of Nothing -> [c]
--               Just (UnitPrice ma)  -> c:(concatMap amountCommodities $ amounts ma)
--               Just (TotalPrice ma) -> c:(concatMap amountCommodities $ amounts ma)

-- | Get an ordered list of amounts in this journal which can
-- influence canonical amount display styles. Those amounts are, in
-- the following order:
--
-- * amounts in market price (P) directives (in parse order)
-- * posting amounts in transactions (in parse order)
-- * the amount in the final default commodity (D) directive
--
-- Transaction price amounts (posting amounts' aprice field) are not included.
--
journalStyleInfluencingAmounts :: Journal -> [Amount]
journalStyleInfluencingAmounts j =
  dbg7 "journalStyleInfluencingAmounts" $
  catMaybes $ concat [
   [mdefaultcommodityamt]
  ,map (Just . pdamount) $ jpricedirectives j
  ,map Just . concatMap (amountsRaw . pamount) $ journalPostings j
  ]
  where
    -- D's amount style isn't actually stored as an amount, make it into one
    mdefaultcommodityamt =
      case jparsedefaultcommodity j of
        Just (symbol,style) -> Just nullamt{acommodity=symbol,astyle=style}
        Nothing -> Nothing

-- overcomplicated/unused amount traversal stuff
--
-- | Get an ordered list of 'AmountStyle's from the amounts in this
-- journal which influence canonical amount display styles. See
-- traverseJournalAmounts.
-- journalAmounts :: Journal -> [Amount]
-- journalAmounts = getConst . traverseJournalAmounts (Const . (:[]))
--
-- | Apply a transformation to the journal amounts traversed by traverseJournalAmounts.
-- overJournalAmounts :: (Amount -> Amount) -> Journal -> Journal
-- overJournalAmounts f = runIdentity . traverseJournalAmounts (Identity . f)
--
-- | A helper that traverses over most amounts in the journal,
-- in particular the ones which influence canonical amount display styles,
-- processing them with the given applicative function.
--
-- These include, in the following order:
--
-- * the amount in the final default commodity (D) directive
-- * amounts in market price (P) directives (in parse order)
-- * posting amounts in transactions (in parse order)
--
-- Transaction price amounts, which may be embedded in posting amounts
-- (the aprice field), are left intact but not traversed/processed.
--
-- traverseJournalAmounts :: Applicative f => (Amount -> f Amount) -> Journal -> f Journal
-- traverseJournalAmounts f j =
--   recombine <$> (traverse . dcamt) f (jparsedefaultcommodity j)
--             <*> (traverse . pdamt) f (jpricedirectives j)
--             <*> (traverse . tps . traverse . pamt . amts . traverse) f (jtxns j)
--   where
--     recombine pds txns = j { jpricedirectives = pds, jtxns = txns }
--     -- a bunch of traversals
--     dcamt g pd         = (\mdc -> case mdc of Nothing -> Nothing
--                                               Just ((c,stpd{pdamount =amt}
--                          ) <$> g (pdamount pd)
--     pdamt g pd         = (\amt -> pd{pdamount =amt}) <$> g (pdamount pd)
--     tps   g t          = (\ps  -> t {tpostings=ps }) <$> g (tpostings t)
--     pamt  g p          = (\amt -> p {pamount  =amt}) <$> g (pamount p)
--     amts  g (Mixed as) = Mixed <$> g as

-- | The fully specified date span enclosing the dates (primary or secondary)
-- of all this journal's transactions and postings, or DateSpan Nothing Nothing
-- if there are none.
journalDateSpan :: Bool -> Journal -> DateSpan
journalDateSpan False = journalDateSpanHelper $ Just PrimaryDate
journalDateSpan True  = journalDateSpanHelper $ Just SecondaryDate

-- | The fully specified date span enclosing the dates (primary and secondary)
-- of all this journal's transactions and postings, or DateSpan Nothing Nothing
-- if there are none.
journalDateSpanBothDates :: Journal -> DateSpan
journalDateSpanBothDates = journalDateSpanHelper Nothing

-- | A helper for journalDateSpan which takes Maybe WhichDate directly. Nothing
-- uses both primary and secondary dates.
journalDateSpanHelper :: Maybe WhichDate -> Journal -> DateSpan
journalDateSpanHelper whichdate j =
    DateSpan (minimumMay dates) (addDays 1 <$> maximumMay dates)
  where
    dates    = pdates ++ tdates
    tdates   = concatMap gettdate ts
    pdates   = concatMap getpdate $ concatMap tpostings ts
    ts       = jtxns j
    gettdate t = case whichdate of
        Just PrimaryDate   -> [tdate t]
        Just SecondaryDate -> [fromMaybe (tdate t) $ tdate2 t]
        Nothing            -> tdate t : maybeToList (tdate2 t)
    getpdate p = case whichdate of
        Just PrimaryDate   -> maybeToList $ pdate p
        Just SecondaryDate -> maybeToList $ pdate2 p <|> pdate p
        Nothing            -> catMaybes [pdate p, pdate2 p]

-- | The earliest of this journal's transaction and posting dates, or
-- Nothing if there are none.
journalStartDate :: Bool -> Journal -> Maybe Day
journalStartDate secondary j = b where DateSpan b _ = journalDateSpan secondary j

-- | The "exclusive end date" of this journal: the day following its latest transaction 
-- or posting date, or Nothing if there are none.
journalEndDate :: Bool -> Journal -> Maybe Day
journalEndDate secondary j = e where DateSpan _ e = journalDateSpan secondary j

-- | The latest of this journal's transaction and posting dates, or
-- Nothing if there are none.
journalLastDay :: Bool -> Journal -> Maybe Day
journalLastDay secondary j = addDays (-1) <$> journalEndDate secondary j

-- | Apply the pivot transformation to all postings in a journal,
-- replacing their account name by their value for the given field or tag.
journalPivot :: Text -> Journal -> Journal
journalPivot fieldortagname j = j{jtxns = map (transactionPivot fieldortagname) . jtxns $ j}

-- | Replace this transaction's postings' account names with the value
-- of the given field or tag, if any.
transactionPivot :: Text -> Transaction -> Transaction
transactionPivot fieldortagname t = t{tpostings = map (postingPivot fieldortagname) . tpostings $ t}

-- | Replace this posting's account name with the value
-- of the given field or tag, if any, otherwise the empty string.
postingPivot :: Text -> Posting -> Posting
postingPivot fieldortagname p = p{paccount = pivotedacct, poriginal = Just $ originalPosting p}
  where
    pivotedacct
      | Just t <- ptransaction p, fieldortagname == "code"        = tcode t
      | Just t <- ptransaction p, fieldortagname == "description" = tdescription t
      | Just t <- ptransaction p, fieldortagname == "payee"       = transactionPayee t
      | Just t <- ptransaction p, fieldortagname == "note"        = transactionNote t
      | Just t <- ptransaction p, fieldortagname == "status"      = T.pack . show . tstatus $ t
      | Just (_, value) <- postingFindTag fieldortagname p        = value
      | otherwise                                                 = ""

postingFindTag :: TagName -> Posting -> Maybe (TagName, TagValue)
postingFindTag tagname p = find ((tagname==) . fst) $ postingAllTags p

-- | Apply some account aliases to all posting account names in the journal, as described by accountNameApplyAliases.
-- This can fail due to a bad replacement pattern in a regular expression alias.
journalApplyAliases :: [AccountAlias] -> Journal -> Either RegexError Journal
-- short circuit the common case, just in case there's a performance impact from txnTieKnot etc.
journalApplyAliases [] j = Right j
journalApplyAliases aliases j = 
  case mapM (transactionApplyAliases aliases) $ jtxns j of
    Right ts -> Right j{jtxns = ts}
    Left err -> Left err

-- -- | Build a database of market prices in effect on the given date,
-- -- from the journal's price directives.
-- journalPrices :: Day -> Journal -> Prices
-- journalPrices d = toPrices d . jpricedirectives

-- -- | Render a market price as a P directive.
-- showPriceDirectiveDirective :: PriceDirective -> String
-- showPriceDirectiveDirective pd = unwords
--     [ "P"
--     , showDate (pddate pd)
--     , T.unpack (pdcommodity pd)
--     , (showAmount . amountSetPrecision maxprecision) (pdamount pd
--     )
--     ]

-- debug helpers
-- traceAmountPrecision a = trace (show $ map (precision . acommodity) $ amounts a) a
-- tracePostingsCommodities ps = trace (show $ map ((map (precision . acommodity) . amounts) . pamount) ps) ps

-- tests
--
-- A sample journal for testing, similar to examples/sample.journal.
-- Provide an option to either use explicit amounts or missing amounts, for testing purposes.
--
-- 2008/01/01 income
--     assets:bank:checking  $1
--     income:salary
--
-- 2008/06/01 gift
--     assets:bank:checking  $1
--     income:gifts
--
-- 2008/06/02 save
--     assets:bank:saving  $1
--     assets:bank:checking
--
-- 2008/06/03 * eat & shop
--     expenses:food      $1
--     expenses:supplies  $1
--     assets:cash
--
-- 2008/10/01 take a loan
--     assets:bank:checking $1
--     liabilities:debts    $-1
--
-- 2008/12/31 * pay off
--     liabilities:debts  $1
--     assets:bank:checking

samplejournal = samplejournalMaybeExplicit True

samplejournalMaybeExplicit :: Bool -> Journal
samplejournalMaybeExplicit explicit = nulljournal
         {jtxns = [
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=fromGregorian 2008 01 01,
             tdate2=Nothing,
             tstatus=Unmarked,
             tcode="",
             tdescription="income",
             tcomment="",
             ttags=[],
             tpostings=
                 ["assets:bank:checking" `post` usd 1
                 ,"income:salary" `post` if explicit then usd (-1) else missingamt
                 ],
             tprecedingcomment=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=fromGregorian 2008 06 01,
             tdate2=Nothing,
             tstatus=Unmarked,
             tcode="",
             tdescription="gift",
             tcomment="",
             ttags=[],
             tpostings=
                 ["assets:bank:checking" `post` usd 1
                 ,"income:gifts" `post` if explicit then usd (-1) else missingamt
                 ],
             tprecedingcomment=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=fromGregorian 2008 06 02,
             tdate2=Nothing,
             tstatus=Unmarked,
             tcode="",
             tdescription="save",
             tcomment="",
             ttags=[],
             tpostings=
                 ["assets:bank:saving" `post` usd 1
                 ,"assets:bank:checking" `post` if explicit then usd (-1) else missingamt
                 ],
             tprecedingcomment=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=fromGregorian 2008 06 03,
             tdate2=Nothing,
             tstatus=Cleared,
             tcode="",
             tdescription="eat & shop",
             tcomment="",
             ttags=[],
             tpostings=["expenses:food" `post` usd 1
                       ,"expenses:supplies" `post` usd 1
                       ,"assets:cash" `post` if explicit then usd (-2) else missingamt
                       ],
             tprecedingcomment=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=fromGregorian 2008 10 01,
             tdate2=Nothing,
             tstatus=Unmarked,
             tcode="",
             tdescription="take a loan",
             tcomment="",
             ttags=[],
             tpostings=["assets:bank:checking" `post` usd 1
                       ,"liabilities:debts" `post` usd (-1)
                       ],
             tprecedingcomment=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=fromGregorian 2008 12 31,
             tdate2=Nothing,
             tstatus=Unmarked,
             tcode="",
             tdescription="pay off",
             tcomment="",
             ttags=[],
             tpostings=["liabilities:debts" `post` usd 1
                       ,"assets:bank:checking" `post` if explicit then usd (-1) else missingamt
                       ],
             tprecedingcomment=""
           }
          ]
         }

tests_Journal = testGroup "Journal" [

   testCase "journalDateSpan" $
    journalDateSpan True nulljournal{
      jtxns = [nulltransaction{tdate = fromGregorian 2014 02 01
                              ,tpostings = [posting{pdate=Just (fromGregorian 2014 01 10)}]
                              }
              ,nulltransaction{tdate = fromGregorian 2014 09 01
                              ,tpostings = [posting{pdate2=Just (fromGregorian 2014 10 10)}]
                              }
              ]
      }
    @?= (DateSpan (Just $ fromGregorian 2014 1 10) (Just $ fromGregorian 2014 10 11))

  ,testGroup "standard account type queries" $
    let
      j = samplejournal
      journalAccountNamesMatching :: Query -> Journal -> [AccountName]
      journalAccountNamesMatching q = filter (q `matchesAccount`) . journalAccountNames
      namesfrom qfunc = journalAccountNamesMatching (qfunc j) j
    in [testCase "assets"      $ assertEqual "" ["assets","assets:bank","assets:bank:checking","assets:bank:saving","assets:cash"]
         (namesfrom journalAssetAccountQuery)
       ,testCase "cash"        $ assertEqual "" ["assets:bank","assets:bank:checking","assets:bank:saving","assets:cash"]
         (namesfrom journalCashAccountQuery)
       ,testCase "liabilities" $ assertEqual "" ["liabilities","liabilities:debts"]
         (namesfrom journalLiabilityAccountQuery)
       ,testCase "equity"      $ assertEqual "" []
         (namesfrom journalEquityAccountQuery)
       ,testCase "income"      $ assertEqual "" ["income","income:gifts","income:salary"]
         (namesfrom journalRevenueAccountQuery)
       ,testCase "expenses"    $ assertEqual "" ["expenses","expenses:food","expenses:supplies"]
         (namesfrom journalExpenseAccountQuery)
       ]
  ]
