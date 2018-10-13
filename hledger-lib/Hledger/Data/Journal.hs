{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{-|

A 'Journal' is a set of transactions, plus optional related data.  This is
hledger's primary data object. It is usually parsed from a journal file or
other data format (see "Hledger.Read").

-}

module Hledger.Data.Journal (
  -- * Parsing helpers
  addMarketPrice,
  addTransactionModifier,
  addPeriodicTransaction,
  addTransaction,
  journalBalanceTransactions,
  journalApplyCommodityStyles,
  commodityStylesFromAmounts,
  journalCommodityStyles,
  journalConvertAmountsToCost,
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
  -- * Querying
  journalAccountNamesUsed,
  journalAccountNamesImplied,
  journalAccountNamesDeclared,
  journalAccountNamesDeclaredOrUsed,
  journalAccountNamesDeclaredOrImplied,
  journalAccountNames,
  -- journalAmountAndPriceCommodities,
  journalAmounts,
  overJournalAmounts,
  traverseJournalAmounts,
  -- journalCanonicalCommodities,
  journalDateSpan,
  journalDescriptions,
  journalFilePath,
  journalFilePaths,
  journalTransactionAt,
  journalNextTransaction,
  journalPrevTransaction,
  journalPostings,
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
  matchpats,
  nulljournal,
  journalCheckBalanceAssertions,
  journalNumberAndTieTransactions,
  journalUntieTransactions,
  -- * Tests
  samplejournal,
  tests_Journal,
)
where
import Control.Applicative (Const(..))
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import qualified Control.Monad.Reader as R
import Control.Monad.ST
import Data.Array.ST
import Data.Functor.Identity (Identity(..))
import qualified Data.HashTable.ST.Cuckoo as HT
import Data.List
import Data.List.Extra (groupSort)
import Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Ord
import qualified Data.Semigroup as Sem
import Data.Text (Text)
import qualified Data.Text as T
import Safe (headMay, headDef)
import Data.Time.Calendar
import Data.Tree
import System.Time (ClockTime(TOD))
import Text.Printf
import qualified Data.Map as M

import Hledger.Utils 
import Hledger.Data.Types
import Hledger.Data.AccountName
import Hledger.Data.Amount
import Hledger.Data.Dates
import Hledger.Data.Transaction
import Hledger.Data.Posting
import Hledger.Query


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
--                      ,show $ jmarketprices j
--                      ,show $ jfinalcommentlines j
--                      ,show $ jparsestate j
--                      ,show $ map fst $ jfiles j
--                      ]

-- The monoid instance for Journal is useful for two situations.
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
instance Sem.Semigroup Journal where
  j1 <> j2 = Journal {
     jparsedefaultyear          = jparsedefaultyear          j2
    ,jparsedefaultcommodity     = jparsedefaultcommodity     j2
    ,jparseparentaccounts       = jparseparentaccounts       j2
    ,jparsealiases              = jparsealiases              j2
    -- ,jparsetransactioncount     = jparsetransactioncount     j1 +  jparsetransactioncount     j2
    ,jparsetimeclockentries     = jparsetimeclockentries j1 <> jparsetimeclockentries j2
    ,jincludefilestack          = jincludefilestack          j2
    ,jdeclaredaccounts          = jdeclaredaccounts          j1 <> jdeclaredaccounts          j2
    ,jdeclaredaccounttypes      = jdeclaredaccounttypes      j1 <> jdeclaredaccounttypes      j2
    ,jcommodities               = jcommodities               j1 <> jcommodities               j2
    ,jinferredcommodities       = jinferredcommodities       j1 <> jinferredcommodities       j2
    ,jmarketprices              = jmarketprices              j1 <> jmarketprices              j2
    ,jtxnmodifiers              = jtxnmodifiers              j1 <> jtxnmodifiers              j2
    ,jperiodictxns              = jperiodictxns              j1 <> jperiodictxns              j2
    ,jtxns                      = jtxns                      j1 <> jtxns                      j2
    ,jfinalcommentlines         = jfinalcommentlines         j2  -- XXX discards j1's ?
    ,jfiles                     = jfiles                     j1 <> jfiles                     j2
    ,jlastreadtime              = max (jlastreadtime j1) (jlastreadtime j2)
    }

instance Monoid Journal where
  mempty = nulljournal
#if !(MIN_VERSION_base(4,11,0))
  -- This is redundant starting with base-4.11 / GHC 8.4.
  mappend = (Sem.<>)
#endif

nulljournal :: Journal
nulljournal = Journal {
   jparsedefaultyear          = Nothing
  ,jparsedefaultcommodity     = Nothing
  ,jparseparentaccounts       = []
  ,jparsealiases              = []
  -- ,jparsetransactioncount     = 0
  ,jparsetimeclockentries     = []
  ,jincludefilestack          = []
  ,jdeclaredaccounts          = []
  ,jdeclaredaccounttypes      = M.empty
  ,jcommodities               = M.empty
  ,jinferredcommodities       = M.empty
  ,jmarketprices              = []
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

addMarketPrice :: MarketPrice -> Journal -> Journal
addMarketPrice h j = j { jmarketprices = h : jmarketprices j }

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

-- | Unique transaction descriptions used in this journal.
journalDescriptions :: Journal -> [Text]
journalDescriptions = nub . sort . map tdescription . jtxns

-- | All postings from this journal's transactions, in order.
journalPostings :: Journal -> [Posting]
journalPostings = concatMap tpostings . jtxns

-- | Sorted unique account names posted to by this journal's transactions.
journalAccountNamesUsed :: Journal -> [AccountName]
journalAccountNamesUsed = accountNamesFromPostings . journalPostings

-- | Sorted unique account names implied by this journal's transactions - 
-- accounts posted to and all their implied parent accounts.
journalAccountNamesImplied :: Journal -> [AccountName]
journalAccountNamesImplied = expandAccountNames . journalAccountNamesUsed

-- | Sorted unique account names declared by account directives in this journal.
journalAccountNamesDeclared :: Journal -> [AccountName]
journalAccountNamesDeclared = nub . sort . jdeclaredaccounts

-- | Sorted unique account names declared by account directives or posted to
-- by transactions in this journal.
journalAccountNamesDeclaredOrUsed :: Journal -> [AccountName]
journalAccountNamesDeclaredOrUsed j = nub $ sort $ journalAccountNamesDeclared j ++ journalAccountNamesUsed j

-- | Sorted unique account names declared by account directives, or posted to
-- or implied as parents by transactions in this journal.
journalAccountNamesDeclaredOrImplied :: Journal -> [AccountName]
journalAccountNamesDeclaredOrImplied j = nub $ sort $ journalAccountNamesDeclared j ++ journalAccountNamesImplied j

-- | Convenience/compatibility alias for journalAccountNamesDeclaredOrImplied.
journalAccountNames :: Journal -> [AccountName]
journalAccountNames = journalAccountNamesDeclaredOrImplied 

journalAccountNameTree :: Journal -> Tree AccountName
journalAccountNameTree = accountNameTreeFrom . journalAccountNames

-- queries for standard account types

-- | Get a query for accounts of a certain type (Asset, Liability..) in this journal.  
-- The query will match all accounts which were declared as that type by account directives, 
-- plus all their subaccounts which have not been declared as a different type. 
-- If no accounts were declared as this type, the query will instead match accounts 
-- with names matched by the provided case-insensitive regular expression.
journalAccountTypeQuery :: AccountType -> Regexp -> Journal -> Query
journalAccountTypeQuery atype fallbackregex j =
  case M.lookup atype (jdeclaredaccounttypes j) of
    Nothing -> Acct fallbackregex
    Just as ->
      -- XXX Query isn't able to match account type since that requires extra info from the journal. 
      -- So we do a hacky search by name instead.
      And [ 
         Or $ map (Acct . accountNameToAccountRegex) as
        ,Not $ Or $ map (Acct . accountNameToAccountRegex) differentlytypedsubs
        ]
      where
        differentlytypedsubs = concat 
          [subs | (t,bs) <- M.toList (jdeclaredaccounttypes j)
              , t /= atype
              , let subs = [b | b <- bs, any (`isAccountNamePrefixOf` b) as]
          ]

-- | A query for accounts in this journal which have been
-- declared as Asset by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression 
-- @^assets?(:|$)@.
journalAssetAccountQuery :: Journal -> Query
journalAssetAccountQuery = journalAccountTypeQuery Asset "^assets?(:|$)"

-- | A query for accounts in this journal which have been
-- declared as Liability by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression 
-- @^(debts?|liabilit(y|ies))(:|$)@.
journalLiabilityAccountQuery :: Journal -> Query
journalLiabilityAccountQuery = journalAccountTypeQuery Liability "^(debts?|liabilit(y|ies))(:|$)"

-- | A query for accounts in this journal which have been
-- declared as Equity by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression 
-- @^equity(:|$)@.
journalEquityAccountQuery :: Journal -> Query
journalEquityAccountQuery = journalAccountTypeQuery Equity "^equity(:|$)"

-- | A query for accounts in this journal which have been
-- declared as Revenue by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression 
-- @^(income|revenue)s?(:|$)@.
journalRevenueAccountQuery :: Journal -> Query
journalRevenueAccountQuery = journalAccountTypeQuery Revenue "^(income|revenue)s?(:|$)"

-- | A query for accounts in this journal which have been
-- declared as Expense by account directives, or otherwise for
-- accounts with names matched by the case-insensitive regular expression 
-- @^(income|revenue)s?(:|$)@.
journalExpenseAccountQuery  :: Journal -> Query
journalExpenseAccountQuery = journalAccountTypeQuery Expense "^expenses?(:|$)"

-- | A query for Asset, Liability & Equity accounts in this journal.
-- Cf <http://en.wikipedia.org/wiki/Chart_of_accounts#Balance_Sheet_Accounts>.
journalBalanceSheetAccountQuery  :: Journal -> Query
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

-- | A query for Cash (-equivalent) accounts in this journal (ie,
-- accounts which appear on the cashflow statement.)  This is currently
-- hard-coded to be all the Asset accounts except for those with names 
-- containing the case-insensitive regular expression @(receivable|:A/R|:fixed)@.
journalCashAccountQuery  :: Journal -> Query
journalCashAccountQuery j = And [journalAssetAccountQuery j, Not $ Acct "(receivable|:A/R|:fixed)"]

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
filterPostingAmount q p@Posting{pamount=Mixed as} = p{pamount=Mixed $ filter (q `matchesAmount`) as}

filterTransactionPostings :: Query -> Transaction -> Transaction
filterTransactionPostings q t@Transaction{tpostings=ps} = t{tpostings=filter (q `matchesPosting`) ps}


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

-- | Reverse parsed data to normal order. This is used for post-parse
-- processing, since data is added to the head of the list during
-- parsing.
journalReverse :: Journal -> Journal
journalReverse j =
  j {jfiles            = reverse $ jfiles j
    ,jdeclaredaccounts = reverse $ jdeclaredaccounts j
    ,jtxns             = reverse $ jtxns j
    ,jtxnmodifiers     = reverse $ jtxnmodifiers j
    ,jperiodictxns     = reverse $ jperiodictxns j
    ,jmarketprices     = reverse $ jmarketprices j
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

-- | Check any balance assertions in the journal and return an error
-- message if any of them fail.
journalCheckBalanceAssertions :: Journal -> Either String Journal
journalCheckBalanceAssertions j =
  runST $ journalBalanceTransactionsST 
    True 
    j 
    (return ())
    (\_ _ -> return ()) 
    (const $ return j)

-- | Check a posting's balance assertion and return an error if it
-- fails.
checkBalanceAssertion :: Posting -> MixedAmount -> Either String ()
checkBalanceAssertion p@Posting{ pbalanceassertion = Just ass } bal
  | isReallyZeroAmount diff = Right ()
  | True    = Left err
    where amt = baamount ass
          assertedcomm = acommodity amt
          actualbal = fromMaybe nullamt $ find ((== assertedcomm) . acommodity) (amounts bal)
          diff = amt - actualbal
          diffplus | isNegativeAmount diff == False = "+"
                   | otherwise = ""
          err = printf (unlines
                        [ "balance assertion error%s",
                          "after posting:",
                          "%s",
                          "balance assertion details:",
                          "date:       %s",
                          "account:    %s",
                          "commodity:  %s",
                          "calculated: %s",
                          "asserted:   %s (difference: %s)"
                        ])
            (case ptransaction p of
               Nothing -> ":" -- shouldn't happen
               Just t ->  printf " in %s:\nin transaction:\n%s"
                          (showGenericSourcePos pos) (chomp $ showTransaction t) :: String
                            where pos = baposition $ fromJust $ pbalanceassertion p)
            (showPostingLine p)
            (showDate $ postingDate p)
            (T.unpack $ paccount p) -- XXX pack
            assertedcomm
            (showAmount actualbal)
            (showAmount amt)
            (diffplus ++ showAmount diff)
checkBalanceAssertion _ _ = Right ()

-- | Fill in any missing amounts and check that all journal transactions
-- balance, or return an error message. This is done after parsing all
-- amounts and applying canonical commodity styles, since balancing
-- depends on display precision. Reports only the first error encountered.
journalBalanceTransactions :: Bool -> Journal -> Either String Journal
journalBalanceTransactions assrt j =
  runST $ journalBalanceTransactionsST 
    assrt -- check balance assertions also ?
    (journalNumberTransactions j) -- journal to process
    (newArray_ (1, genericLength $ jtxns j) :: forall s. ST s (STArray s Integer Transaction)) -- initialise state
    (\arr tx -> writeArray arr (tindex tx) tx)    -- update state
    (fmap (\txns -> j{ jtxns = txns}) . getElems) -- summarise state

-- | Helper used by 'journalBalanceTransactions' and 'journalCheckBalanceAssertions'.
journalBalanceTransactionsST ::
  Bool
  -> Journal
  -> ST s txns                        -- ^ initialise state
  -> (txns -> Transaction -> ST s ()) -- ^ update state
  -> (txns -> ST s a)                 -- ^ summarise state
  -> ST s (Either String a)
journalBalanceTransactionsST assrt j createStore storeIn extract =
  runExceptT $ do
    bals <- lift $ HT.newSized size
    txStore <- lift $ createStore
    let env = Env bals 
                  (storeIn txStore) 
                  assrt
                  (Just $ journalCommodityStyles j)
    flip R.runReaderT env $ do
      dated <- fmap snd . sortBy (comparing fst) . concat
                <$> mapM' discriminateByDate (jtxns j)
      mapM' checkInferAndRegisterAmounts dated
    lift $ extract txStore
    where 
      size = genericLength $ journalPostings j

-- | Monad transformer stack with a reference to a mutable hashtable
-- of current account balances and a mutable array of finished
-- transactions in original parsing order.
type CurrentBalancesModifier s = R.ReaderT (Env s) (ExceptT String (ST s))

-- | Environment for 'CurrentBalancesModifier'
data Env s = Env { eBalances :: HT.HashTable s AccountName MixedAmount
                 , eStoreTx  :: Transaction -> ST s ()
                 , eAssrt    :: Bool
                 , eStyles   :: Maybe (M.Map CommoditySymbol AmountStyle) 
                 }

-- | This converts a transaction into a list of transactions or
-- postings whose dates have to be considered when checking 
-- balance assertions and handled by 'checkInferAndRegisterAmounts'.
--
-- Transaction without balance assignments can be balanced and stored
-- immediately and their (possibly) dated postings are returned.
--
-- Transaction with balance assignments are only supported if no
-- posting has a 'pdate' value. Supported transactions will be
-- returned unchanged and balanced and stored later in 'checkInferAndRegisterAmounts'.
discriminateByDate :: Transaction
  -> CurrentBalancesModifier s [(Day, Either Posting Transaction)]
discriminateByDate tx
  | null (assignmentPostings tx) = do
      styles <- R.reader $ eStyles
      balanced <- lift $ ExceptT $ return $ balanceTransaction styles tx
      storeTransaction balanced
      return $ 
        fmap (postingDate &&& (Left . removePrices)) $ tpostings $ balanced
  | True                         = do
      when (any (isJust . pdate) $ tpostings tx) $
        throwError $ unlines $
        ["postings may not have both a custom date and a balance assignment."
        ,"Write the posting amount explicitly, or remove the posting date:\n"
        , showTransaction tx]
      return 
        [(tdate tx, Right $ tx { tpostings = removePrices <$> tpostings tx })]

-- | This function takes an object describing changes to
-- account balances on a single day - either a single posting 
-- (from an already balanced transaction without assignments)
-- or a whole transaction with assignments (which is required to 
-- have no posting with pdate set).
--
-- For a single posting, there is not much to do. Only add its amount
-- to its account and check the assertion, if there is one. This
-- functionality is provided by 'addAmountAndCheckBalance'.
--
-- For a whole transaction, it loops over all postings, and performs
-- 'addAmountAndCheckBalance', if there is an amount. If there is no
-- amount, the amount is inferred by the assertion or left empty if
-- there is no assertion. Then, the transaction is balanced, the
-- inferred amount added to the balance (all in 'balanceTransactionUpdate') 
-- and the resulting transaction with no missing amounts is stored 
-- in the array, for later retrieval.
--
-- Again in short:
--
-- 'Left Posting': Check the balance assertion and update the
--  account balance. If the amount is empty do nothing.  this can be
--  the case e.g. for virtual postings
--
-- 'Right Transaction': Loop over all postings, infer their amounts
-- and then balance and store the transaction.
checkInferAndRegisterAmounts :: Either Posting Transaction
                             -> CurrentBalancesModifier s ()
checkInferAndRegisterAmounts (Left p) =
  void $ addAmountAndCheckBalance return p
checkInferAndRegisterAmounts (Right oldTx) = do
  let ps = tpostings oldTx
  styles <- R.reader $ eStyles
  newPostings <- forM ps $ addAmountAndCheckBalance inferFromAssignment
  storeTransaction =<< balanceTransactionUpdate
    (fmap void . addToBalance) styles oldTx { tpostings = newPostings }
  where
    inferFromAssignment :: Posting -> CurrentBalancesModifier s Posting
    inferFromAssignment p = do
      let acc = paccount p
      case pbalanceassertion p of
        Just ba -> do
          old <- liftModifier $ \Env{ eBalances = bals } -> HT.lookup bals acc
          let amt = baamount ba
              assertedcomm = acommodity amt
          diff <- setMixedBalance acc $
            Mixed [amt] + filterMixedAmount (\a -> acommodity a /= assertedcomm) (fromMaybe nullmixedamt old)
          fullPosting diff p
        Nothing -> return p
    fullPosting amt p = return p
      { pamount = amt
      , porigin = Just $ originalPosting p
      }

-- | Adds a posting's amount to the posting's account balance and
-- checks a possible balance assertion. Or if there is no amount,
-- runs the supplied fallback action.
addAmountAndCheckBalance :: 
     (Posting -> CurrentBalancesModifier s Posting) -- ^ action if posting has no amount
  -> Posting
  -> CurrentBalancesModifier s Posting
addAmountAndCheckBalance _ p | hasAmount p = do
  newAmt <- addToBalance (paccount p) $ pamount p
  assrt <- R.reader eAssrt
  lift $ when assrt $ ExceptT $ return $ checkBalanceAssertion p newAmt
  return p
addAmountAndCheckBalance fallback p = fallback p

-- | Sets all commodities comprising an account's balance to the given
-- amounts and returns the difference from the previous balance.
setMixedBalance :: AccountName -> MixedAmount -> CurrentBalancesModifier s MixedAmount
setMixedBalance acc amt = liftModifier $ \Env{ eBalances = bals } -> do
  old <- HT.lookup bals acc
  HT.insert bals acc amt
  return $ maybe amt (amt -) old

-- | Adds an amount to an account's balance and returns the resulting balance.
addToBalance :: AccountName -> MixedAmount -> CurrentBalancesModifier s MixedAmount
addToBalance acc amt = liftModifier $ \Env{ eBalances = bals } -> do
  new <- maybe amt (+ amt) <$> HT.lookup bals acc
  HT.insert bals acc new
  return new

-- | Stores a transaction in the transaction array in original parsing order.
storeTransaction :: Transaction -> CurrentBalancesModifier s ()
storeTransaction tx = liftModifier $ ($tx) . eStoreTx

-- | Helper function.
liftModifier :: (Env s -> ST s a) -> CurrentBalancesModifier s a
liftModifier f = R.ask >>= lift . lift . f

-- | Choose and apply a consistent display format to the posting
-- amounts in each commodity. Each commodity's format is specified by
-- a commodity format directive, or otherwise inferred from posting
-- amounts as in hledger < 0.28.
journalApplyCommodityStyles :: Journal -> Journal
journalApplyCommodityStyles j@Journal{jtxns=ts, jmarketprices=mps} = j''
    where
      j' = journalInferCommodityStyles j
      styles = journalCommodityStyles j'
      j'' = j'{jtxns=map fixtransaction ts, jmarketprices=map fixmarketprice mps}
      fixtransaction t@Transaction{tpostings=ps} = t{tpostings=map fixposting ps}
      fixposting p@Posting{pamount=a} = p{pamount=styleMixedAmount styles a}
      fixmarketprice mp@MarketPrice{mpamount=a} = mp{mpamount=styleAmount styles a}

-- | Get all the amount styles defined in this journal, either declared by 
-- a commodity directive or inferred from amounts, as a map from symbol to style. 
-- Styles declared by commodity directives take precedence, and these also are
-- guaranteed to know their decimal point character.
journalCommodityStyles :: Journal -> M.Map CommoditySymbol AmountStyle
journalCommodityStyles j = declaredstyles <> inferredstyles
  where
    declaredstyles = M.mapMaybe cformat $ jcommodities j
    inferredstyles = jinferredcommodities j

-- | Collect and save inferred amount styles for each commodity based on
-- the posting amounts in that commodity (excluding price amounts), ie:
-- "the format of the first amount, adjusted to the highest precision of all amounts".
journalInferCommodityStyles :: Journal -> Journal
journalInferCommodityStyles j =
  j{jinferredcommodities =
    commodityStylesFromAmounts $
    dbg8 "journalInferCommmodityStyles using amounts" $ journalAmounts j}

-- | Given a list of amounts in parse order, build a map from their commodity names
-- to standard commodity display formats.
commodityStylesFromAmounts :: [Amount] -> M.Map CommoditySymbol AmountStyle
commodityStylesFromAmounts amts = M.fromList commstyles
  where
    commamts = groupSort [(acommodity as, as) | as <- amts]
    commstyles = [(c, canonicalStyleFrom $ map astyle as) | (c,as) <- commamts]

-- | Given an ordered list of amount styles, choose a canonical style.
-- That is: the style of the first, and the maximum precision of all.
canonicalStyleFrom :: [AmountStyle] -> AmountStyle
canonicalStyleFrom [] = amountstyle
canonicalStyleFrom ss@(first:_) =
  first{asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
  where
    mgrps = maybe Nothing Just $ headMay $ catMaybes $ map asdigitgroups ss
    -- precision is maximum of all precisions
    prec = maximumStrict $ map asprecision ss
    mdec  = Just $ headDef '.' $ catMaybes $ map asdecimalpoint ss
    -- precision is that of first amount with a decimal point
    -- (mdec, prec) =
    --   case filter (isJust . asdecimalpoint) ss of
    --   (s:_) -> (asdecimalpoint s, asprecision s)
    --   []    -> (Just '.', 0)

-- -- | Apply this journal's historical price records to unpriced amounts where possible.
-- journalApplyMarketPrices :: Journal -> Journal
-- journalApplyMarketPrices j@Journal{jtxns=ts} = j{jtxns=map fixtransaction ts}
--     where
--       fixtransaction t@Transaction{tdate=d, tpostings=ps} = t{tpostings=map fixposting ps}
--        where
--         fixposting p@Posting{pamount=a} = p{pamount=fixmixedamount a}
--         fixmixedamount (Mixed as) = Mixed $ map fixamount as
--         fixamount = fixprice
--         fixprice a@Amount{price=Just _} = a
--         fixprice a@Amount{commodity=c} = a{price=maybe Nothing (Just . UnitPrice) $ journalMarketPriceFor j d c}

-- -- | Get the price for a commodity on the specified day from the price database, if known.
-- -- Does only one lookup step, ie will not look up the price of a price.
-- journalMarketPriceFor :: Journal -> Day -> CommoditySymbol -> Maybe MixedAmount
-- journalMarketPriceFor j d CommoditySymbol{symbol=s} = do
--   let ps = reverse $ filter ((<= d).mpdate) $ filter ((s==).hsymbol) $ sortBy (comparing mpdate) $ jmarketprices j
--   case ps of (MarketPrice{mpamount=a}:_) -> Just a
--              _ -> Nothing

-- | Convert all this journal's amounts to cost by applying their prices, if any.
journalConvertAmountsToCost :: Journal -> Journal
journalConvertAmountsToCost j@Journal{jtxns=ts} = j{jtxns=map fixtransaction ts}
    where
      -- similar to journalApplyCommodityStyles
      fixtransaction t@Transaction{tpostings=ps} = t{tpostings=map fixposting ps}
      fixposting p@Posting{pamount=a} = p{pamount=fixmixedamount a}
      fixmixedamount (Mixed as) = Mixed $ map fixamount as
      fixamount = styleAmount styles . costOfAmount
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

-- | Get an ordered list of the amounts in this journal which will
-- influence amount style canonicalisation. These are:
--
-- * amounts in market price directives (in parse order)
-- * amounts in postings (in parse order)
--
-- Amounts in default commodity directives also influence
-- canonicalisation, but earlier, as amounts are parsed.
-- Amounts in posting prices are not used for canonicalisation.
--
journalAmounts :: Journal -> [Amount]
journalAmounts = getConst . traverseJournalAmounts (Const . (:[]))

-- | Maps over all of the amounts in the journal
overJournalAmounts :: (Amount -> Amount) -> Journal -> Journal
overJournalAmounts f = runIdentity . traverseJournalAmounts (Identity . f)

-- | Traverses over all ofthe amounts in the journal, in the order
-- indicated by 'journalAmounts'.
traverseJournalAmounts
    :: Applicative f
    => (Amount -> f Amount)
    -> Journal -> f Journal
traverseJournalAmounts f j =
    recombine <$> (traverse . mpa) f (jmarketprices j)
              <*> (traverse . tp . traverse . pamt . maa . traverse) f (jtxns j)
  where
    recombine mps txns = j { jmarketprices = mps, jtxns = txns }
    -- a bunch of traversals
    mpa  g mp = (\amt -> mp { mpamount  = amt }) <$> g (mpamount mp)
    tp   g t  = (\ps  -> t  { tpostings = ps  }) <$> g (tpostings t)
    pamt g p  = (\amt -> p  { pamount   = amt }) <$> g (pamount p)
    maa  g (Mixed as) = Mixed <$> g as

-- | The fully specified date span enclosing the dates (primary or secondary)
-- of all this journal's transactions and postings, or DateSpan Nothing Nothing
-- if there are none.
journalDateSpan :: Bool -> Journal -> DateSpan
journalDateSpan secondary j
    | null ts   = DateSpan Nothing Nothing
    | otherwise = DateSpan (Just earliest) (Just $ addDays 1 latest)
    where
      earliest = minimumStrict dates
      latest   = maximumStrict dates
      dates    = pdates ++ tdates
      tdates   = map (if secondary then transactionDate2 else tdate) ts
      pdates   = concatMap (catMaybes . map (if secondary then (Just . postingDate2) else pdate) . tpostings) ts
      ts       = jtxns j

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
postingPivot fieldortagname p = p{paccount = pivotedacct, porigin = Just $ originalPosting p}
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

-- Misc helpers

-- | Check if a set of hledger account/description filter patterns matches the
-- given account name or entry description.  Patterns are case-insensitive
-- regular expressions. Prefixed with not:, they become anti-patterns.
matchpats :: [String] -> String -> Bool
matchpats pats str =
    (null positives || any match positives) && (null negatives || not (any match negatives))
    where
      (negatives,positives) = partition isnegativepat pats
      match "" = True
      match pat = regexMatchesCI (abspat pat) str

negateprefix = "not:"

isnegativepat = (negateprefix `isPrefixOf`)

abspat pat = if isnegativepat pat then drop (length negateprefix) pat else pat

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
Right samplejournal = journalBalanceTransactions False $
         nulljournal
         {jtxns = [
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=parsedate "2008/01/01",
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
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=parsedate "2008/06/01",
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
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=parsedate "2008/06/02",
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
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=parsedate "2008/06/03",
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
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=parsedate "2008/10/01",
             tdate2=Nothing,
             tstatus=Unmarked,
             tcode="",
             tdescription="take a loan",
             tcomment="",
             ttags=[],
             tpostings=["assets:bank:checking" `post` usd 1
                       ,"liabilities:debts" `post` usd (-1)
                       ],
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=parsedate "2008/12/31",
             tdate2=Nothing,
             tstatus=Unmarked,
             tcode="",
             tdescription="pay off",
             tcomment="",
             ttags=[],
             tpostings=["liabilities:debts" `post` usd 1
                       ,"assets:bank:checking" `post` usd (-1)
                       ],
             tpreceding_comment_lines=""
           }
          ]
         }

tests_Journal = tests "Journal" [

   test "journalDateSpan" $
    journalDateSpan True nulljournal{
      jtxns = [nulltransaction{tdate = parsedate "2014/02/01"
                              ,tpostings = [posting{pdate=Just (parsedate "2014/01/10")}]
                              }
              ,nulltransaction{tdate = parsedate "2014/09/01"
                              ,tpostings = [posting{pdate2=Just (parsedate "2014/10/10")}]
                              }
              ]
      }
    `is` (DateSpan (Just $ fromGregorian 2014 1 10) (Just $ fromGregorian 2014 10 11))

  ,tests "standard account type queries" $
    let
      j = samplejournal
      journalAccountNamesMatching :: Query -> Journal -> [AccountName]
      journalAccountNamesMatching q = filter (q `matchesAccount`) . journalAccountNames
      namesfrom qfunc = journalAccountNamesMatching (qfunc j) j
    in [
       test "assets"      $ expectEq (namesfrom journalAssetAccountQuery)     ["assets","assets:bank","assets:bank:checking","assets:bank:saving","assets:cash"]
      ,test "liabilities" $ expectEq (namesfrom journalLiabilityAccountQuery) ["liabilities","liabilities:debts"]
      ,test "equity"      $ expectEq (namesfrom journalEquityAccountQuery)    []
      ,test "income"      $ expectEq (namesfrom journalRevenueAccountQuery)    ["income","income:gifts","income:salary"]
      ,test "expenses"    $ expectEq (namesfrom journalExpenseAccountQuery)   ["expenses","expenses:food","expenses:supplies"]
    ]

  ]
