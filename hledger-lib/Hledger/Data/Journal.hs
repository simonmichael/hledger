{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

A 'Journal' is a set of transactions, plus optional related data.  This is
hledger's primary data object. It is usually parsed from a journal file or
other data format (see "Hledger.Read").

-}

module Hledger.Data.Journal (
  -- * Parsing helpers
  addPriceDirective,
  addTransactionModifier,
  addPeriodicTransaction,
  addTransaction,
  journalBalanceTransactions,
  journalInferMarketPricesFromTransactions,
  journalApplyCommodityStyles,
  commodityStylesFromAmounts,
  journalCommodityStyles,
  journalToCost,
  journalReverse,
  journalSetLastReadTime,
  journalPivot,
  -- * Filtering
  filterJournalTransactions,
  filterJournalPostings,
  filterJournalAmounts,
  filterTransactionAmounts,
  filterTransactionPostings,
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
  journalAccountNames,
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
  journalDescriptions,
  journalFilePath,
  journalFilePaths,
  journalTransactionAt,
  journalNextTransaction,
  journalPrevTransaction,
  journalPostings,
  journalTransactionsSimilarTo,
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
  -- * Misc
  canonicalStyleFrom,
  nulljournal,
  journalCheckBalanceAssertions,
  journalNumberAndTieTransactions,
  journalUntieTransactions,
  journalModifyTransactions,
  journalApplyAliases,
  -- * Tests
  samplejournal,
  tests_Journal,
)
where

import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import "extra" Control.Monad.Extra (whenM)
import Control.Monad.Reader as R
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, getElems, newListArray, writeArray)
import Data.Char (toUpper, isDigit)
import Data.Default (Default(..))
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.HashTable.Class as H (toList)
import qualified Data.HashTable.ST.Cuckoo as H
import Data.List (find, foldl', sortOn)
import Data.List.Extra (nubSort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe, maybeToList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Safe (headMay, headDef, maximumMay, minimumMay)
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Tree (Tree, flatten)
import System.Time (ClockTime(TOD))
import Text.Printf (printf)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.AccountName
import Hledger.Data.Amount
import Hledger.Data.Dates
import Hledger.Data.Transaction
import Hledger.Data.TransactionModifier
import Hledger.Data.Posting
import Hledger.Query
import Data.List (sortBy)


-- try to make Journal ppShow-compatible
-- instance Show ClockTime where
--   show t = "<ClockTime>"
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
    ,jdeclaredaccounttypes      = jdeclaredaccounttypes      j1 <> jdeclaredaccounttypes      j2
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
  ,jdeclaredaccounttypes      = M.empty
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
  ,jlastreadtime              = TOD 0 0
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

journalAccountNameTree :: Journal -> Tree AccountName
journalAccountNameTree = accountNameTreeFrom . journalAccountNamesDeclaredOrImplied

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

-- queries for standard account types

-- | Get a query for accounts of the specified types in this journal. 
-- Account types include Asset, Liability, Equity, Revenue, Expense, Cash.
-- For each type, if no accounts were declared with this type, the query 
-- will instead match accounts with names matched by the case-insensitive 
-- regular expression provided as a fallback.
-- The query will match all accounts which were declared as one of
-- these types (by account directives with the type: tag), plus all their 
-- subaccounts which have not been declared as some other type.
journalAccountTypeQuery :: [AccountType] -> Regexp -> Journal -> Query
journalAccountTypeQuery atypes fallbackregex Journal{jdeclaredaccounttypes} =
  let
    declaredacctsoftype :: [AccountName] =
      concat $ mapMaybe (`M.lookup` jdeclaredaccounttypes) atypes
  in case declaredacctsoftype of
    [] -> Acct fallbackregex
    as -> And $ [ Or acctnameRegexes ]
            ++ if null differentlyTypedRegexes then [] else [ Not $ Or differentlyTypedRegexes ]
      where
        -- XXX Query isn't able to match account type since that requires extra info from the journal.
        -- So we do a hacky search by name instead.
        acctnameRegexes = map (Acct . accountNameToAccountRegex) as
        differentlyTypedRegexes = map (Acct . accountNameToAccountRegex) differentlytypedsubs

        differentlytypedsubs = concat
          [subs | (t,bs) <- M.toList jdeclaredaccounttypes
              , not $ t `elem` atypes
              , let subs = [b | b <- bs, any (`isAccountNamePrefixOf` b) as]
          ]

-- | A query for accounts in this journal which have been
-- declared as Asset (or Cash, a subtype of Asset) by account directives, 
-- or otherwise for accounts with names matched by the case-insensitive 
-- regular expression @^assets?(:|$)@.
journalAssetAccountQuery :: Journal -> Query
journalAssetAccountQuery j = 
  Or [
     journalAccountTypeQuery [Asset] (toRegexCI' "^assets?(:|$)") j
    ,journalCashAccountOnlyQuery j
  ]

-- | A query for accounts in this journal which have been
-- declared as Asset (and not Cash) by account directives, 
-- or otherwise for accounts with names matched by the case-insensitive 
-- regular expression @^assets?(:|$)@.
journalAssetNonCashAccountQuery :: Journal -> Query
journalAssetNonCashAccountQuery j = 
  journalAccountTypeQuery [Asset] (toRegexCI' "^assets?(:|$)") j

-- | A query for Cash (liquid asset) accounts in this journal, ie accounts
-- declared as Cash by account directives, or otherwise Asset accounts whose 
-- names do not include the case-insensitive regular expression 
-- @(investment|receivable|:A/R|:fixed)@.
journalCashAccountQuery  :: Journal -> Query
journalCashAccountQuery j =
  case M.lookup Cash (jdeclaredaccounttypes j) of
    Just _  -> journalCashAccountOnlyQuery j
    Nothing ->
      -- no Cash accounts are declared; query for Asset accounts and exclude some of them
      And [ journalAssetNonCashAccountQuery j, Not . Acct $ toRegexCI' "(investment|receivable|:A/R|:fixed)" ]

-- | A query for accounts in this journal specifically declared as Cash by 
-- account directives, or otherwise the None query.
journalCashAccountOnlyQuery  :: Journal -> Query
journalCashAccountOnlyQuery j = 
  case M.lookup Cash (jdeclaredaccounttypes j) of
    Just _  -> 
      -- Cash accounts are declared; get a query for them (the fallback regex won't be used)
      journalAccountTypeQuery [Cash] notused j
        where notused = error' "journalCashAccountOnlyQuery: this should not have happened!"  -- PARTIAL:
    Nothing -> None

-- | A query for accounts in this journal which have been
-- declared as Liability by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression
-- @^(debts?|liabilit(y|ies))(:|$)@.
journalLiabilityAccountQuery :: Journal -> Query
journalLiabilityAccountQuery = journalAccountTypeQuery [Liability] (toRegexCI' "^(debts?|liabilit(y|ies))(:|$)")

-- | A query for accounts in this journal which have been
-- declared as Equity by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression
-- @^equity(:|$)@.
journalEquityAccountQuery :: Journal -> Query
journalEquityAccountQuery = journalAccountTypeQuery [Equity] (toRegexCI' "^equity(:|$)")

-- | A query for accounts in this journal which have been
-- declared as Revenue by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression
-- @^(income|revenue)s?(:|$)@.
journalRevenueAccountQuery :: Journal -> Query
journalRevenueAccountQuery = journalAccountTypeQuery [Revenue] (toRegexCI' "^(income|revenue)s?(:|$)")

-- | A query for accounts in this journal which have been
-- declared as Expense by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression
-- @^expenses?(:|$)@.
journalExpenseAccountQuery  :: Journal -> Query
journalExpenseAccountQuery = journalAccountTypeQuery [Expense] (toRegexCI' "^expenses?(:|$)")

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

-- Various kinds of filtering on journals. We do it differently depending
-- on the command.

-------------------------------------------------------------------------------
-- filtering V2

-- | Keep only transactions matching the query expression.
filterJournalTransactions :: Query -> Journal -> Journal
filterJournalTransactions q j@Journal{jtxns=ts} = j{jtxns=filter (q `matchesTransaction`) ts}

-- | Keep only postings matching the query expression.
-- This can leave unbalanced transactions.
filterJournalPostings :: Query -> Journal -> Journal
filterJournalPostings q j@Journal{jtxns=ts} = j{jtxns=map (filterTransactionPostings q) ts}

-- | Within each posting's amount, keep only the parts matching the query.
-- This can leave unbalanced transactions.
filterJournalAmounts :: Query -> Journal -> Journal
filterJournalAmounts q j@Journal{jtxns=ts} = j{jtxns=map (filterTransactionAmounts q) ts}

-- | Filter out all parts of this transaction's amounts which do not match the query.
-- This can leave the transaction unbalanced.
filterTransactionAmounts :: Query -> Transaction -> Transaction
filterTransactionAmounts q t@Transaction{tpostings=ps} = t{tpostings=map (filterPostingAmount q) ps}

-- | Filter out all parts of this posting's amount which do not match the query.
filterPostingAmount :: Query -> Posting -> Posting
filterPostingAmount q p@Posting{pamount=as} = p{pamount=filterMixedAmount (q `matchesAmount`) as}

filterTransactionPostings :: Query -> Transaction -> Transaction
filterTransactionPostings q t@Transaction{tpostings=ps} = t{tpostings=filter (q `matchesPosting`) ps}

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
journalSetLastReadTime :: ClockTime -> Journal -> Journal
journalSetLastReadTime t j = j{ jlastreadtime = t }


journalNumberAndTieTransactions = journalTieTransactions . journalNumberTransactions

-- | Number (set the tindex field) this journal's transactions, counting upward from 1.
journalNumberTransactions :: Journal -> Journal
journalNumberTransactions j@Journal{jtxns=ts} = j{jtxns=map (\(i,t) -> t{tindex=i}) $ zip [1..] ts}

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
  case modifyTransactions d (jtxnmodifiers j) (jtxns j) of
    Right ts -> Right j{jtxns=ts}
    Left err -> Left err

-- | Check any balance assertions in the journal and return an error message
-- if any of them fail (or if the transaction balancing they require fails).
journalCheckBalanceAssertions :: Journal -> Maybe String
journalCheckBalanceAssertions = either Just (const Nothing) . journalBalanceTransactions def

-- "Transaction balancing", including: inferring missing amounts,
-- applying balance assignments, checking transaction balancedness,
-- checking balance assertions, respecting posting dates. These things
-- are all interdependent.
-- WARN tricky algorithm and code ahead. 
--
-- Code overview as of 20190219, this could/should be simplified/documented more:
--  parseAndFinaliseJournal['] (Cli/Utils.hs), journalAddForecast (Common.hs), journalAddBudgetGoalTransactions (BudgetReport.hs), tests (BalanceReport.hs)
--   journalBalanceTransactions
--    runST
--     runExceptT
--      balanceTransaction (Transaction.hs)
--       balanceTransactionHelper
--      runReaderT
--       balanceTransactionAndCheckAssertionsB
--        addAmountAndCheckAssertionB
--        addOrAssignAmountAndCheckAssertionB
--        balanceTransactionHelper (Transaction.hs)
--  uiCheckBalanceAssertions d ui@UIState{aopts=UIOpts{cliopts_=copts}, ajournal=j} (ErrorScreen.hs)
--   journalCheckBalanceAssertions
--    journalBalanceTransactions
--  transactionWizard, postingsBalanced (Add.hs), tests (Transaction.hs)
--   balanceTransaction (Transaction.hs)  XXX hledger add won't allow balance assignments + missing amount ?

-- | Monad used for statefully balancing/amount-inferring/assertion-checking
-- a sequence of transactions.
-- Perhaps can be simplified, or would a different ordering of layers make sense ?
-- If you see a way, let us know.
type Balancing s = ReaderT (BalancingState s) (ExceptT String (ST s))

-- | The state used while balancing a sequence of transactions.
data BalancingState s = BalancingState {
   -- read only
   bsStyles       :: Maybe (M.Map CommoditySymbol AmountStyle)  -- ^ commodity display styles
  ,bsUnassignable :: S.Set AccountName                          -- ^ accounts in which balance assignments may not be used
  ,bsAssrt        :: Bool                                       -- ^ whether to check balance assertions
   -- mutable
  ,bsBalances     :: H.HashTable s AccountName MixedAmount      -- ^ running account balances, initially empty
  ,bsTransactions :: STArray s Integer Transaction              -- ^ a mutable array of the transactions being balanced
    -- (for efficiency ? journalBalanceTransactions says: not strictly necessary but avoids a sort at the end I think)
  }

-- | Access the current balancing state, and possibly modify the mutable bits,
-- lifting through the Except and Reader layers into the Balancing monad.
withRunningBalance :: (BalancingState s -> ST s a) -> Balancing s a
withRunningBalance f = ask >>= lift . lift . f

-- | Get this account's current exclusive running balance.
getRunningBalanceB :: AccountName -> Balancing s MixedAmount
getRunningBalanceB acc = withRunningBalance $ \BalancingState{bsBalances} -> do
  fromMaybe nullmixedamt <$> H.lookup bsBalances acc

-- | Add this amount to this account's exclusive running balance.
-- Returns the new running balance.
addToRunningBalanceB :: AccountName -> MixedAmount -> Balancing s MixedAmount
addToRunningBalanceB acc amt = withRunningBalance $ \BalancingState{bsBalances} -> do
  old <- fromMaybe nullmixedamt <$> H.lookup bsBalances acc
  let new = maPlus old amt
  H.insert bsBalances acc new
  return new

-- | Set this account's exclusive running balance to this amount.
-- Returns the change in exclusive running balance.
setRunningBalanceB :: AccountName -> MixedAmount -> Balancing s MixedAmount
setRunningBalanceB acc amt = withRunningBalance $ \BalancingState{bsBalances} -> do
  old <- fromMaybe nullmixedamt <$> H.lookup bsBalances acc
  H.insert bsBalances acc amt
  return $ maMinus amt old

-- | Set this account's exclusive running balance to whatever amount
-- makes its *inclusive* running balance (the sum of exclusive running
-- balances of this account and any subaccounts) be the given amount.
-- Returns the change in exclusive running balance.
setInclusiveRunningBalanceB :: AccountName -> MixedAmount -> Balancing s MixedAmount
setInclusiveRunningBalanceB acc newibal = withRunningBalance $ \BalancingState{bsBalances} -> do
  oldebal  <- fromMaybe nullmixedamt <$> H.lookup bsBalances acc
  allebals <- H.toList bsBalances
  let subsibal =  -- sum of any subaccounts' running balances
        maSum . map snd $ filter ((acc `isAccountNamePrefixOf`).fst) allebals
  let newebal = maMinus newibal subsibal
  H.insert bsBalances acc newebal
  return $ maMinus newebal oldebal

-- | Update (overwrite) this transaction in the balancing state.
updateTransactionB :: Transaction -> Balancing s ()
updateTransactionB t = withRunningBalance $ \BalancingState{bsTransactions}  ->
  void $ writeArray bsTransactions (tindex t) t

-- | Infer any missing amounts (to satisfy balance assignments and
-- to balance transactions) and check that all transactions balance
-- and (optional) all balance assertions pass. Or return an error message
-- (just the first error encountered).
--
-- Assumes journalInferCommodityStyles has been called, since those
-- affect transaction balancing.
--
-- This does multiple things at once because amount inferring, balance
-- assignments, balance assertions and posting dates are interdependent.
journalBalanceTransactions :: BalancingOpts -> Journal -> Either String Journal
journalBalanceTransactions bopts' j' =
  let
    -- ensure transactions are numbered, so we can store them by number
    j@Journal{jtxns=ts} = journalNumberTransactions j'
    -- display precisions used in balanced checking
    styles = Just $ journalCommodityStyles j
    bopts = bopts'{commodity_styles_=styles}
    -- balance assignments will not be allowed on these
    txnmodifieraccts = S.fromList $ map paccount $ concatMap tmpostingrules $ jtxnmodifiers j
  in
    runST $ do
      -- We'll update a mutable array of transactions as we balance them,
      -- not strictly necessary but avoids a sort at the end I think.
      balancedtxns <- newListArray (1, toInteger $ length ts) ts

      -- Infer missing posting amounts, check transactions are balanced,
      -- and check balance assertions. This is done in two passes:
      runExceptT $ do

        -- 1. Step through the transactions, balancing the ones which don't have balance assignments
        -- and leaving the others for later. The balanced ones are split into their postings.
        -- The postings and not-yet-balanced transactions remain in the same relative order.
        psandts :: [Either Posting Transaction] <- fmap concat $ forM ts $ \case
          t | null $ assignmentPostings t -> case balanceTransaction bopts t of
              Left  e  -> throwError e
              Right t' -> do
                lift $ writeArray balancedtxns (tindex t') t'
                return $ map Left $ tpostings t'
          t -> return [Right t]

        -- 2. Sort these items by date, preserving the order of same-day items,
        -- and step through them while keeping running account balances,
        runningbals <- lift $ H.newSized (length $ journalAccountNamesUsed j)
        flip runReaderT (BalancingState styles txnmodifieraccts (not $ ignore_assertions_ bopts) runningbals balancedtxns) $ do
          -- performing balance assignments in, and balancing, the remaining transactions,
          -- and checking balance assertions as each posting is processed.
          void $ mapM' balanceTransactionAndCheckAssertionsB $ sortOn (either postingDate tdate) psandts

        ts' <- lift $ getElems balancedtxns
        return j{jtxns=ts'}

-- | This function is called statefully on each of a date-ordered sequence of
-- 1. fully explicit postings from already-balanced transactions and
-- 2. not-yet-balanced transactions containing balance assignments.
-- It executes balance assignments and finishes balancing the transactions,
-- and checks balance assertions on each posting as it goes.
-- An error will be thrown if a transaction can't be balanced
-- or if an illegal balance assignment is found (cf checkIllegalBalanceAssignment).
-- Transaction prices are removed, which helps eg balance-assertions.test: 15. Mix different commodities and assignments.
-- This stores the balanced transactions in case 2 but not in case 1.
balanceTransactionAndCheckAssertionsB :: Either Posting Transaction -> Balancing s ()
balanceTransactionAndCheckAssertionsB (Left p@Posting{}) =
  -- update the account's running balance and check the balance assertion if any
  void . addAmountAndCheckAssertionB $ postingStripPrices p
balanceTransactionAndCheckAssertionsB (Right t@Transaction{tpostings=ps}) = do
  -- make sure we can handle the balance assignments
  mapM_ checkIllegalBalanceAssignmentB ps
  -- for each posting, infer its amount from the balance assignment if applicable,
  -- update the account's running balance and check the balance assertion if any
  ps' <- mapM (addOrAssignAmountAndCheckAssertionB . postingStripPrices) ps
  -- infer any remaining missing amounts, and make sure the transaction is now fully balanced
  styles <- R.reader bsStyles
  case balanceTransactionHelper balancingOpts{commodity_styles_=styles} t{tpostings=ps'} of
    Left err -> throwError err
    Right (t', inferredacctsandamts) -> do
      -- for each amount just inferred, update the running balance
      mapM_ (uncurry addToRunningBalanceB) inferredacctsandamts
      -- and save the balanced transaction.
      updateTransactionB t'

-- | If this posting has an explicit amount, add it to the account's running balance.
-- If it has a missing amount and a balance assignment, infer the amount from, and
-- reset the running balance to, the assigned balance.
-- If it has a missing amount and no balance assignment, leave it for later.
-- Then test the balance assertion if any.
addOrAssignAmountAndCheckAssertionB :: Posting -> Balancing s Posting
addOrAssignAmountAndCheckAssertionB p@Posting{paccount=acc, pamount=amt, pbalanceassertion=mba}
  -- an explicit posting amount
  | hasAmount p = do
      newbal <- addToRunningBalanceB acc amt
      whenM (R.reader bsAssrt) $ checkBalanceAssertionB p newbal
      return p

  -- no explicit posting amount, but there is a balance assignment
  | Just BalanceAssertion{baamount,batotal,bainclusive} <- mba = do
      newbal <- if batotal
                   -- a total balance assignment (==, all commodities)
                   then return $ mixedAmount baamount
                   -- a partial balance assignment (=, one commodity)
                   else do
                     oldbalothercommodities <- filterMixedAmount ((acommodity baamount /=) . acommodity) <$> getRunningBalanceB acc
                     return $ maAddAmount oldbalothercommodities baamount
      diff <- (if bainclusive then setInclusiveRunningBalanceB else setRunningBalanceB) acc newbal
      let p' = p{pamount=diff, poriginal=Just $ originalPosting p}
      whenM (R.reader bsAssrt) $ checkBalanceAssertionB p' newbal
      return p'

  -- no explicit posting amount, no balance assignment
  | otherwise = return p

-- | Add the posting's amount to its account's running balance, and
-- optionally check the posting's balance assertion if any.
-- The posting is expected to have an explicit amount (otherwise this does nothing).
-- Adding and checking balance assertions are tightly paired because we
-- need to see the balance as it stands after each individual posting.
addAmountAndCheckAssertionB :: Posting -> Balancing s Posting
addAmountAndCheckAssertionB p | hasAmount p = do
  newbal <- addToRunningBalanceB (paccount p) $ pamount p
  whenM (R.reader bsAssrt) $ checkBalanceAssertionB p newbal
  return p
addAmountAndCheckAssertionB p = return p

-- | Check a posting's balance assertion against the given actual balance, and
-- return an error if the assertion is not satisfied.
-- If the assertion is partial, unasserted commodities in the actual balance
-- are ignored; if it is total, they will cause the assertion to fail.
checkBalanceAssertionB :: Posting -> MixedAmount -> Balancing s ()
checkBalanceAssertionB p@Posting{pbalanceassertion=Just (BalanceAssertion{baamount,batotal})} actualbal =
    forM_ (baamount : otheramts) $ \amt -> checkBalanceAssertionOneCommodityB p amt actualbal
  where
    assertedcomm = acommodity baamount
    otheramts | batotal   = map (\a -> a{aquantity=0}) . amountsRaw
                          $ filterMixedAmount ((/=assertedcomm).acommodity) actualbal
              | otherwise = []
checkBalanceAssertionB _ _ = return ()

-- | Does this (single commodity) expected balance match the amount of that
-- commodity in the given (multicommodity) actual balance ? If not, returns a
-- balance assertion failure message based on the provided posting.  To match,
-- the amounts must be exactly equal (display precision is ignored here).
-- If the assertion is inclusive, the expected amount is compared with the account's
-- subaccount-inclusive balance; otherwise, with the subaccount-exclusive balance.
checkBalanceAssertionOneCommodityB :: Posting -> Amount -> MixedAmount -> Balancing s ()
checkBalanceAssertionOneCommodityB p@Posting{paccount=assertedacct} assertedamt actualbal = do
  let isinclusive = maybe False bainclusive $ pbalanceassertion p
  actualbal' <-
    if isinclusive
    then
      -- sum the running balances of this account and any of its subaccounts seen so far
      withRunningBalance $ \BalancingState{bsBalances} ->
        H.foldM
          (\ibal (acc, amt) -> return $
            if assertedacct==acc || assertedacct `isAccountNamePrefixOf` acc then maPlus ibal amt else ibal)
          nullmixedamt
          bsBalances
    else return actualbal
  let
    assertedcomm    = acommodity assertedamt
    actualbalincomm = headDef nullamt . amountsRaw . filterMixedAmountByCommodity assertedcomm $ actualbal'
    pass =
      aquantity
        -- traceWith (("asserted:"++).showAmountDebug)
        assertedamt ==
      aquantity
        -- traceWith (("actual:"++).showAmountDebug)
        actualbalincomm

    errmsg = printf (unlines
                  [ "balance assertion: %s",
                    "\nassertion details:",
                    "date:       %s",
                    "account:    %s%s",
                    "commodity:  %s",
                    -- "display precision:  %d",
                    "calculated: %s", -- (at display precision: %s)",
                    "asserted:   %s", -- (at display precision: %s)",
                    "difference: %s"
                  ])
      (case ptransaction p of
         Nothing -> "?" -- shouldn't happen
         Just t ->  printf "%s\ntransaction:\n%s"
                      (showGenericSourcePos pos)
                      (textChomp $ showTransaction t)
                      :: String
                      where
                        pos = baposition $ fromJust $ pbalanceassertion p
      )
      (showDate $ postingDate p)
      (T.unpack $ paccount p) -- XXX pack
      (if isinclusive then " (and subs)" else "" :: String)
      assertedcomm
      -- (asprecision $ astyle actualbalincommodity)  -- should be the standard display precision I think
      (show $ aquantity actualbalincomm)
      -- (showAmount actualbalincommodity)
      (show $ aquantity assertedamt)
      -- (showAmount assertedamt)
      (show $ aquantity assertedamt - aquantity actualbalincomm)

  when (not pass) $ throwError errmsg

-- | Throw an error if this posting is trying to do an illegal balance assignment.
checkIllegalBalanceAssignmentB :: Posting -> Balancing s ()
checkIllegalBalanceAssignmentB p = do
  checkBalanceAssignmentPostingDateB p
  checkBalanceAssignmentUnassignableAccountB p

-- XXX these should show position. annotateErrorWithTransaction t ?

-- | Throw an error if this posting is trying to do a balance assignment and
-- has a custom posting date (which makes amount inference too hard/impossible).
checkBalanceAssignmentPostingDateB :: Posting -> Balancing s ()
checkBalanceAssignmentPostingDateB p =
  when (hasBalanceAssignment p && isJust (pdate p)) $
    throwError . T.unpack $ T.unlines
      ["postings which are balance assignments may not have a custom date."
      ,"Please write the posting amount explicitly, or remove the posting date:"
      ,""
      ,maybe (T.unlines $ showPostingLines p) showTransaction $ ptransaction p
      ]

-- | Throw an error if this posting is trying to do a balance assignment and
-- the account does not allow balance assignments (eg because it is referenced
-- by a transaction modifier, which might generate additional postings to it).
checkBalanceAssignmentUnassignableAccountB :: Posting -> Balancing s ()
checkBalanceAssignmentUnassignableAccountB p = do
  unassignable <- R.asks bsUnassignable
  when (hasBalanceAssignment p && paccount p `S.member` unassignable) $
    throwError . T.unpack $ T.unlines
      ["balance assignments cannot be used with accounts which are"
      ,"posted to by transaction modifier rules (auto postings)."
      ,"Please write the posting amount explicitly, or remove the rule."
      ,""
      ,"account: " <> paccount p
      ,""
      ,"transaction:"
      ,""
      ,maybe (T.unlines $ showPostingLines p) showTransaction $ ptransaction p
      ]

--

-- | Choose and apply a consistent display style to the posting
-- amounts in each commodity (see journalCommodityStyles).
-- Can return an error message eg if inconsistent number formats are found.
journalApplyCommodityStyles :: Journal -> Either String Journal
journalApplyCommodityStyles j@Journal{jtxns=ts, jpricedirectives=pds} =
  case journalInferCommodityStyles j of
    Left e   -> Left e
    Right j' -> Right j''
      where
        styles = journalCommodityStyles j'
        j'' = j'{jtxns=map fixtransaction ts
                ,jpricedirectives=map fixpricedirective pds
                }
        fixtransaction t@Transaction{tpostings=ps} = t{tpostings=map fixposting ps}
        fixposting p = p{pamount=styleMixedAmount styles $ pamount p
                        ,pbalanceassertion=fixbalanceassertion <$> pbalanceassertion p}
        -- balance assertion amounts are always displayed (by print) at full precision, per docs
        fixbalanceassertion ba = ba{baamount=styleAmountExceptPrecision styles $ baamount ba}
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
canonicalStyleFrom ss = foldl' canonicalStyle amountstyle ss

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
       dbg4 "jinferredmarketprices" $
       mapMaybe postingInferredmarketPrice $ journalPostings j
   }

-- | Make a market price equivalent to this posting's amount's unit
-- price, if any. If the posting amount is multicommodity, only the
-- first commodity amount is considered.
postingInferredmarketPrice :: Posting -> Maybe MarketPrice
postingInferredmarketPrice p@Posting{pamount} =
    -- convert any total prices to unit prices
    case amountsRaw $ mixedAmountTotalPriceToUnitPrice pamount of
      Amount{acommodity=fromcomm, aprice = Just (UnitPrice Amount{acommodity=tocomm, aquantity=rate})}:_ ->
        Just MarketPrice {
           mpdate = postingDate p
          ,mpfrom = fromcomm
          ,mpto   = tocomm
          ,mprate = rate
          }
      _ -> Nothing

-- | Convert all this journal's amounts to cost using the transaction prices, if any.
-- The journal's commodity styles are applied to the resulting amounts.
journalToCost :: Journal -> Journal
journalToCost j@Journal{jtxns=ts} = j{jtxns=map (transactionToCost styles) ts}
    where
      styles = journalCommodityStyles j

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

-- | The latest of this journal's transaction and posting dates, or
-- Nothing if there are none.
journalEndDate :: Bool -> Journal -> Maybe Day
journalEndDate secondary j = e where DateSpan _ e = journalDateSpan secondary j

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

-- A sample journal for testing, similar to examples/sample.journal:
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
--
Right samplejournal = journalBalanceTransactions def $
         nulljournal
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
                 ,"income:salary" `post` missingamt
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
                 ,"income:gifts" `post` missingamt
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
                 ,"assets:bank:checking" `post` usd (-1)
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
                       ,"assets:cash" `post` missingamt
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
                       ,"assets:bank:checking" `post` usd (-1)
                       ],
             tprecedingcomment=""
           }
          ]
         }

tests_Journal = tests "Journal" [

   test "journalDateSpan" $
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

  ,tests "standard account type queries" $
    let
      j = samplejournal
      journalAccountNamesMatching :: Query -> Journal -> [AccountName]
      journalAccountNamesMatching q = filter (q `matchesAccount`) . journalAccountNames
      namesfrom qfunc = journalAccountNamesMatching (qfunc j) j
    in [
       test "assets"      $ assertEqual "" ["assets","assets:bank","assets:bank:checking","assets:bank:saving","assets:cash"]
         (namesfrom journalAssetAccountQuery)
      ,test "cash"        $ assertEqual "" ["assets","assets:bank","assets:bank:checking","assets:bank:saving","assets:cash"]
        (namesfrom journalCashAccountQuery)
      ,test "liabilities" $ assertEqual "" ["liabilities","liabilities:debts"]
        (namesfrom journalLiabilityAccountQuery)
      ,test "equity"      $ assertEqual "" []
        (namesfrom journalEquityAccountQuery)
      ,test "income"      $ assertEqual "" ["income","income:gifts","income:salary"]
        (namesfrom journalRevenueAccountQuery)
      ,test "expenses"    $ assertEqual "" ["expenses","expenses:food","expenses:supplies"]
        (namesfrom journalExpenseAccountQuery)
    ]

  ,tests "journalBalanceTransactions" [

     test "balance-assignment" $ do
      let ej = journalBalanceTransactions def $
            --2019/01/01
            --  (a)            = 1
            nulljournal{ jtxns = [
              transaction (fromGregorian 2019 01 01) [ vpost' "a" missingamt (balassert (num 1)) ]
            ]}
      assertRight ej
      let Right j = ej
      (jtxns j & head & tpostings & head & pamount & amountsRaw) @?= [num 1]

    ,test "same-day-1" $ do
      assertRight $ journalBalanceTransactions def $
            --2019/01/01
            --  (a)            = 1
            --2019/01/01
            --  (a)          1 = 2
            nulljournal{ jtxns = [
               transaction (fromGregorian 2019 01 01) [ vpost' "a" missingamt (balassert (num 1)) ]
              ,transaction (fromGregorian 2019 01 01) [ vpost' "a" (num 1)    (balassert (num 2)) ]
            ]}

    ,test "same-day-2" $ do
      assertRight $ journalBalanceTransactions def $
            --2019/01/01
            --    (a)                  2 = 2
            --2019/01/01
            --    b                    1
            --    a
            --2019/01/01
            --    a                    0 = 1
            nulljournal{ jtxns = [
               transaction (fromGregorian 2019 01 01) [ vpost' "a" (num 2)    (balassert (num 2)) ]
              ,transaction (fromGregorian 2019 01 01) [
                 post' "b" (num 1)     Nothing
                ,post' "a"  missingamt Nothing
              ]
              ,transaction (fromGregorian 2019 01 01) [ post' "a" (num 0)     (balassert (num 1)) ]
            ]}

    ,test "out-of-order" $ do
      assertRight $ journalBalanceTransactions def $
            --2019/1/2
            --  (a)    1 = 2
            --2019/1/1
            --  (a)    1 = 1
            nulljournal{ jtxns = [
               transaction (fromGregorian 2019 01 02) [ vpost' "a" (num 1)    (balassert (num 2)) ]
              ,transaction (fromGregorian 2019 01 01) [ vpost' "a" (num 1)    (balassert (num 1)) ]
            ]}

    ]

    ,tests "commodityStylesFromAmounts" $ [

      -- Journal similar to the one on #1091:
      -- 2019/09/24
      --     (a)            1,000.00
      -- 
      -- 2019/09/26
      --     (a)             1000,000
      --
      test "1091a" $ do
        commodityStylesFromAmounts [
           nullamt{aquantity=1000, astyle=AmountStyle L False (Precision 3) (Just ',') Nothing}
          ,nullamt{aquantity=1000, astyle=AmountStyle L False (Precision 2) (Just '.') (Just (DigitGroups ',' [3]))}
          ]
         @?=
          -- The commodity style should have period as decimal mark
          -- and comma as digit group mark.
          Right (M.fromList [
            ("", AmountStyle L False (Precision 3) (Just '.') (Just (DigitGroups ',' [3])))
          ])
        -- same journal, entries in reverse order
      ,test "1091b" $ do
        commodityStylesFromAmounts [
           nullamt{aquantity=1000, astyle=AmountStyle L False (Precision 2) (Just '.') (Just (DigitGroups ',' [3]))}
          ,nullamt{aquantity=1000, astyle=AmountStyle L False (Precision 3) (Just ',') Nothing}
          ]
         @?=
          -- The commodity style should have period as decimal mark
          -- and comma as digit group mark.
          Right (M.fromList [
            ("", AmountStyle L False (Precision 3) (Just '.') (Just (DigitGroups ',' [3])))
          ])

     ]

  ]
