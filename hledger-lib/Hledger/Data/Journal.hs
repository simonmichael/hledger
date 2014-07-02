-- {-# LANGUAGE CPP #-}
{-|

A 'Journal' is a set of transactions, plus optional related data.  This is
hledger's primary data object. It is usually parsed from a journal file or
other data format (see "Hledger.Read").

-}

module Hledger.Data.Journal (
  -- * Parsing helpers
  addHistoricalPrice,
  addModifierTransaction,
  addPeriodicTransaction,
  addTimeLogEntry,
  addTransaction,
  journalApplyAliases,
  journalBalanceTransactions,
  journalCanonicaliseAmounts,
  journalConvertAmountsToCost,
  journalFinalise,
  -- * Filtering
  filterJournalTransactions,
  filterJournalPostings,
  filterJournalAmounts,
  filterTransactionAmounts,
  filterPostingAmount,
  -- * Querying
  journalAccountNames,
  journalAccountNamesUsed,
  -- journalAmountAndPriceCommodities,
  journalAmounts,
  -- journalCanonicalCommodities,
  journalDateSpan,
  journalDescriptions,
  journalFilePath,
  journalFilePaths,
  journalPostings,
  -- * Standard account types
  journalBalanceSheetAccountQuery,
  journalProfitAndLossAccountQuery,
  journalIncomeAccountQuery,
  journalExpenseAccountQuery,
  journalAssetAccountQuery,
  journalLiabilityAccountQuery,
  journalEquityAccountQuery,
  journalCashAccountQuery,
  -- * Misc
  matchpats,
  nullctx,
  nulljournal,
  -- * Tests
  samplejournal,
  tests_Hledger_Data_Journal,
)
where
import Control.Monad
import Data.List
-- import Data.Map (findWithDefault)
import Data.Maybe
import Data.Ord
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Tree
import Safe (headDef)
import System.Time (ClockTime(TOD))
import Test.HUnit
import Text.Printf
import qualified Data.Map as M

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.AccountName
import Hledger.Data.Amount
-- import Hledger.Data.Commodity
import Hledger.Data.Dates
import Hledger.Data.Transaction
import Hledger.Data.Posting
import Hledger.Data.TimeLog
import Hledger.Query


instance Show Journal where
  show j
    | debugLevel < 3 = printf "Journal %s with %d transactions, %d accounts"
             (journalFilePath j)
             (length (jtxns j) +
              length (jmodifiertxns j) +
              length (jperiodictxns j))
             (length accounts)
    | debugLevel < 6 = printf "Journal %s with %d transactions, %d accounts: %s"
             (journalFilePath j)
             (length (jtxns j) +
              length (jmodifiertxns j) +
              length (jperiodictxns j))
             (length accounts)
             (show accounts)
    | otherwise = printf "Journal %s with %d transactions, %d accounts: %s, commodity styles: %s"
             (journalFilePath j)
             (length (jtxns j) +
              length (jmodifiertxns j) +
              length (jperiodictxns j))
             (length accounts)
             (show accounts)
             (show $ jcommoditystyles j)
             -- ++ (show $ journalTransactions l)
             where accounts = flatten $ journalAccountNameTree j

-- showJournalDebug j = unlines [
--                       show j
--                      ,show (jtxns j)
--                      ,show (jmodifiertxns j)
--                      ,show (jperiodictxns j)
--                      ,show $ open_timelog_entries j
--                      ,show $ historical_prices j
--                      ,show $ final_comment_lines j
--                      ,show $ jContext j
--                      ,show $ map fst $ files j
--                      ]

nulljournal :: Journal
nulljournal = Journal { jmodifiertxns = []
                      , jperiodictxns = []
                      , jtxns = []
                      , open_timelog_entries = []
                      , historical_prices = []
                      , final_comment_lines = []
                      , jContext = nullctx
                      , files = []
                      , filereadtime = TOD 0 0
                      , jcommoditystyles = M.fromList []
                      }

nullctx :: JournalContext
nullctx = Ctx { ctxYear = Nothing, ctxCommodityAndStyle = Nothing, ctxAccount = [], ctxAliases = [] }

journalFilePath :: Journal -> FilePath
journalFilePath = fst . mainfile

journalFilePaths :: Journal -> [FilePath]
journalFilePaths = map fst . files

mainfile :: Journal -> (FilePath, String)
mainfile = headDef ("", "") . files

addTransaction :: Transaction -> Journal -> Journal
addTransaction t l0 = l0 { jtxns = t : jtxns l0 }

addModifierTransaction :: ModifierTransaction -> Journal -> Journal
addModifierTransaction mt l0 = l0 { jmodifiertxns = mt : jmodifiertxns l0 }

addPeriodicTransaction :: PeriodicTransaction -> Journal -> Journal
addPeriodicTransaction pt l0 = l0 { jperiodictxns = pt : jperiodictxns l0 }

addHistoricalPrice :: HistoricalPrice -> Journal -> Journal
addHistoricalPrice h l0 = l0 { historical_prices = h : historical_prices l0 }

addTimeLogEntry :: TimeLogEntry -> Journal -> Journal
addTimeLogEntry tle l0 = l0 { open_timelog_entries = tle : open_timelog_entries l0 }

-- | Unique transaction descriptions used in this journal.
journalDescriptions :: Journal -> [String]
journalDescriptions = nub . sort . map tdescription . jtxns

-- | All postings from this journal's transactions, in order.
journalPostings :: Journal -> [Posting]
journalPostings = concatMap tpostings . jtxns

-- | Unique account names posted to in this journal.
journalAccountNamesUsed :: Journal -> [AccountName]
journalAccountNamesUsed = sort . accountNamesFromPostings . journalPostings

-- | Unique account names in this journal, including parent accounts containing no postings.
journalAccountNames :: Journal -> [AccountName]
journalAccountNames = sort . expandAccountNames . journalAccountNamesUsed

journalAccountNameTree :: Journal -> Tree AccountName
journalAccountNameTree = accountNameTreeFrom . journalAccountNames

-- standard account types

-- | A query for Profit & Loss accounts in this journal.
-- Cf <http://en.wikipedia.org/wiki/Chart_of_accounts#Profit_.26_Loss_accounts>.
journalProfitAndLossAccountQuery  :: Journal -> Query
journalProfitAndLossAccountQuery j = Or [journalIncomeAccountQuery j
                                               ,journalExpenseAccountQuery j
                                               ]

-- | A query for Income (Revenue) accounts in this journal.
-- This is currently hard-coded to the case-insensitive regex @^(income|revenue)s?(:|$)@.
journalIncomeAccountQuery  :: Journal -> Query
journalIncomeAccountQuery _ = Acct "^(income|revenue)s?(:|$)"

-- | A query for Expense accounts in this journal.
-- This is currently hard-coded to the case-insensitive regex @^expenses?(:|$)@.
journalExpenseAccountQuery  :: Journal -> Query
journalExpenseAccountQuery _ = Acct "^expenses?(:|$)"

-- | A query for Asset, Liability & Equity accounts in this journal.
-- Cf <http://en.wikipedia.org/wiki/Chart_of_accounts#Balance_Sheet_Accounts>.
journalBalanceSheetAccountQuery  :: Journal -> Query
journalBalanceSheetAccountQuery j = Or [journalAssetAccountQuery j
                                              ,journalLiabilityAccountQuery j
                                              ,journalEquityAccountQuery j
                                              ]

-- | A query for Asset accounts in this journal.
-- This is currently hard-coded to the case-insensitive regex @^assets?(:|$)@.
journalAssetAccountQuery  :: Journal -> Query
journalAssetAccountQuery _ = Acct "^assets?(:|$)"

-- | A query for Liability accounts in this journal.
-- This is currently hard-coded to the case-insensitive regex @^liabilit(y|ies)(:|$)@.
journalLiabilityAccountQuery  :: Journal -> Query
journalLiabilityAccountQuery _ = Acct "^liabilit(y|ies)(:|$)"

-- | A query for Equity accounts in this journal.
-- This is currently hard-coded to the case-insensitive regex @^equity(:|$)@.
journalEquityAccountQuery  :: Journal -> Query
journalEquityAccountQuery _ = Acct "^equity(:|$)"

-- | A query for Cash (-equivalent) accounts in this journal (ie,
-- accounts which appear on the cashflow statement.)  This is currently
-- hard-coded to be all the Asset accounts except for those containing the
-- case-insensitive regex @(receivable|A/R)@.
journalCashAccountQuery  :: Journal -> Query
journalCashAccountQuery j = And [journalAssetAccountQuery j, Not $ Acct "(receivable|A/R)"]

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
filterJournalPostings q j@Journal{jtxns=ts} = j{jtxns=map filtertransactionpostings ts}
    where
      filtertransactionpostings t@Transaction{tpostings=ps} = t{tpostings=filter (q `matchesPosting`) ps}

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
    filterJournalTransactionsByClearedStatus cleared .
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
    filterJournalPostingsByClearedStatus cleared .
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
filterJournalTransactionsByClearedStatus :: Maybe Bool -> Journal -> Journal
filterJournalTransactionsByClearedStatus Nothing j = j
filterJournalTransactionsByClearedStatus (Just val) j@Journal{jtxns=ts} = j{jtxns=filter match ts}
    where match = (==val).tstatus

-- | Keep only postings which have the requested cleared/uncleared status,
-- if there is one.
filterJournalPostingsByClearedStatus :: Maybe Bool -> Journal -> Journal
filterJournalPostingsByClearedStatus Nothing j = j
filterJournalPostingsByClearedStatus (Just c) j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
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

-- | Apply additional account aliases (eg from the command-line) to all postings in a journal.
journalApplyAliases :: [(AccountName,AccountName)] -> Journal -> Journal
journalApplyAliases aliases j@Journal{jtxns=ts} = j{jtxns=map fixtransaction ts}
    where
      fixtransaction t@Transaction{tpostings=ps} = t{tpostings=map fixposting ps}
      fixposting p@Posting{paccount=a} = p{paccount=accountNameApplyAliases aliases a}

-- | Do post-parse processing on a journal to make it ready for use: check
-- all transactions balance, canonicalise amount formats, close any open
-- timelog entries, maybe check balance assertions and so on.
journalFinalise :: ClockTime -> LocalTime -> FilePath -> String -> JournalContext -> Bool -> Journal -> Either String Journal
journalFinalise tclock tlocal path txt ctx assrt j@Journal{files=fs} = do
  (journalBalanceTransactions $
    journalCanonicaliseAmounts $
    journalCloseTimeLogEntries tlocal
    j{files=(path,txt):fs, filereadtime=tclock, jContext=ctx})
  >>= if assrt then journalCheckBalanceAssertions else return

-- | Check any balance assertions in the journal and return an error
-- message if any of them fail.
journalCheckBalanceAssertions :: Journal -> Either String Journal
journalCheckBalanceAssertions j = do
  let postingsByAccount = groupBy (\p1 p2 -> paccount p1 == paccount p2) $
                          sortBy (comparing paccount) $
                          journalPostings j
  forM_ postingsByAccount checkBalanceAssertionsForAccount
  Right j

-- Check any balance assertions in this sequence of postings to a single account.
checkBalanceAssertionsForAccount :: [Posting] -> Either String ()
checkBalanceAssertionsForAccount ps
  | null errs = Right ()
  | otherwise = Left $ head errs
  where
    errs = fst $
           foldl' checkBalanceAssertion ([],nullmixedamt) $
           splitAssertions $
           sortBy (comparing postingDate) ps

-- Given a starting balance, accumulated errors, and a non-null sequence of
-- postings to a single account with a balance assertion in the last:
-- check that the final balance matches the balance assertion.
-- If it does, return the new balance, otherwise add an error to the
-- error list. Intended to be called from a fold.
checkBalanceAssertion :: ([String],MixedAmount) -> [Posting] -> ([String],MixedAmount)
checkBalanceAssertion (errs,bal) ps
  | null ps = (errs,bal)
  | isNothing assertion = (errs,bal)
  |
    -- bal' /= assertedbal  -- MixedAmount's Eq instance currently gets confused by different precisions
    not $ isReallyZeroMixedAmount (bal' - assertedbal)
    -- or, compare only the balance of that commodity, like Ledger
    -- not $ isReallyZeroMixedAmount (filterCommodity () bal' - assertedbal)
      = (errs++[err], bal')
  | otherwise = (errs,bal')
  where
    p = last ps
    assertion = pbalanceassertion p
    Just assertedbal = assertion
    bal' = sum $ [bal] ++ map pamount ps
    err = printf "Balance assertion failed for account %s on %s\n%sAfter posting:\n   %s\nexpected balance is %s, actual balance was %s."
                 (paccount p)
                 (show $ postingDate p)
                 (maybe "" (("In transaction:\n"++).show) $ ptransaction p)
                 (show p)
                 (showMixedAmount assertedbal)
                 (showMixedAmount bal')

-- Given a sequence of postings to a single account, split it into
-- sub-sequences consisting of ordinary postings followed by a single
-- balance-asserting posting. Postings not followed by a balance
-- assertion are discarded.
splitAssertions :: [Posting] -> [[Posting]]
splitAssertions ps
  | null rest = [[]]
  | otherwise = (ps'++[head rest]):splitAssertions (tail rest)
  where
    (ps',rest) = break (isJust . pbalanceassertion) ps
    
-- | Fill in any missing amounts and check that all journal transactions
-- balance, or return an error message. This is done after parsing all
-- amounts and working out the canonical commodities, since balancing
-- depends on display precision. Reports only the first error encountered.
journalBalanceTransactions :: Journal -> Either String Journal
journalBalanceTransactions j@Journal{jtxns=ts, jcommoditystyles=ss} =
  case sequence $ map balance ts of Right ts' -> Right j{jtxns=map txnTieKnot ts'}
                                    Left e    -> Left e
      where balance = balanceTransaction (Just ss)

-- | Convert all the journal's posting amounts (not price amounts) to
-- their canonical display settings. Ie, all amounts in a given
-- commodity will use (a) the display settings of the first, and (b)
-- the greatest precision, of the posting amounts in that commodity.
journalCanonicaliseAmounts :: Journal -> Journal
journalCanonicaliseAmounts j@Journal{jtxns=ts} = j''
    where
      j'' = j'{jtxns=map fixtransaction ts}
      j' = j{jcommoditystyles = canonicalStyles $ journalAmounts j}
      fixtransaction t@Transaction{tpostings=ps} = t{tpostings=map fixposting ps}
      fixposting p@Posting{pamount=a} = p{pamount=fixmixedamount a}
      fixmixedamount (Mixed as) = Mixed $ map fixamount as
      fixamount a@Amount{acommodity=c} = a{astyle=journalCommodityStyle j' c}

-- | Get this journal's canonical amount style for the given commodity, or the null style.
journalCommodityStyle :: Journal -> Commodity -> AmountStyle
journalCommodityStyle j c = M.findWithDefault amountstyle c $ jcommoditystyles j

-- -- | Apply this journal's historical price records to unpriced amounts where possible.
-- journalApplyHistoricalPrices :: Journal -> Journal
-- journalApplyHistoricalPrices j@Journal{jtxns=ts} = j{jtxns=map fixtransaction ts}
--     where
--       fixtransaction t@Transaction{tdate=d, tpostings=ps} = t{tpostings=map fixposting ps}
--        where
--         fixposting p@Posting{pamount=a} = p{pamount=fixmixedamount a}
--         fixmixedamount (Mixed as) = Mixed $ map fixamount as
--         fixamount = fixprice
--         fixprice a@Amount{price=Just _} = a
--         fixprice a@Amount{commodity=c} = a{price=maybe Nothing (Just . UnitPrice) $ journalHistoricalPriceFor j d c}

-- -- | Get the price for a commodity on the specified day from the price database, if known.
-- -- Does only one lookup step, ie will not look up the price of a price.
-- journalHistoricalPriceFor :: Journal -> Day -> Commodity -> Maybe MixedAmount
-- journalHistoricalPriceFor j d Commodity{symbol=s} = do
--   let ps = reverse $ filter ((<= d).hdate) $ filter ((s==).hsymbol) $ sortBy (comparing hdate) $ historical_prices j
--   case ps of (HistoricalPrice{hamount=a}:_) -> Just a
--              _ -> Nothing

-- | Close any open timelog sessions in this journal using the provided current time.
journalCloseTimeLogEntries :: LocalTime -> Journal -> Journal
journalCloseTimeLogEntries now j@Journal{jtxns=ts, open_timelog_entries=es} =
  j{jtxns = ts ++ (timeLogEntriesToTransactions now es), open_timelog_entries = []}

-- | Convert all this journal's amounts to cost by applying their prices, if any.
journalConvertAmountsToCost :: Journal -> Journal
journalConvertAmountsToCost j@Journal{jtxns=ts} = j{jtxns=map fixtransaction ts}
    where
      -- similar to journalCanonicaliseAmounts
      fixtransaction t@Transaction{tpostings=ps} = t{tpostings=map fixposting ps}
      fixposting p@Posting{pamount=a} = p{pamount=fixmixedamount a}
      fixmixedamount (Mixed as) = Mixed $ map fixamount as
      fixamount = canonicaliseAmount (jcommoditystyles j) . costOfAmount

-- -- | Get this journal's unique, display-preference-canonicalised commodities, by symbol.
-- journalCanonicalCommodities :: Journal -> M.Map String Commodity
-- journalCanonicalCommodities j = canonicaliseCommodities $ journalAmountCommodities j

-- -- | Get all this journal's amounts' commodities, in the order parsed.
-- journalAmountCommodities :: Journal -> [Commodity]
-- journalAmountCommodities = map acommodity . concatMap amounts . journalAmounts

-- -- | Get all this journal's amount and price commodities, in the order parsed.
-- journalAmountAndPriceCommodities :: Journal -> [Commodity]
-- journalAmountAndPriceCommodities = concatMap amountCommodities . concatMap amounts . journalAmounts

-- -- | Get this amount's commodity and any commodities referenced in its price.
-- amountCommodities :: Amount -> [Commodity]
-- amountCommodities Amount{acommodity=c,aprice=p} =
--     case p of Nothing -> [c]
--               Just (UnitPrice ma)  -> c:(concatMap amountCommodities $ amounts ma)
--               Just (TotalPrice ma) -> c:(concatMap amountCommodities $ amounts ma)

-- | Get all this journal's (mixed) amounts, in the order parsed.
journalMixedAmounts :: Journal -> [MixedAmount]
journalMixedAmounts = map pamount . journalPostings

-- | Get all this journal's component amounts, roughly in the order parsed.
journalAmounts :: Journal -> [Amount]
journalAmounts = concatMap flatten . journalMixedAmounts where flatten (Mixed as) = as

-- | The fully specified date span enclosing the dates (primary or secondary)
-- of all this journal's transactions and postings, or DateSpan Nothing Nothing
-- if there are none.
journalDateSpan :: Bool -> Journal -> DateSpan
journalDateSpan secondary j
    | null ts   = DateSpan Nothing Nothing
    | otherwise = DateSpan (Just earliest) (Just $ addDays 1 latest)
    where
      earliest = minimum dates
      latest   = maximum dates
      dates    = pdates ++ tdates
      tdates   = map (if secondary then transactionDate2 else tdate) ts
      pdates   = concatMap (catMaybes . map (if secondary then (Just . postingDate2) else pdate) . tpostings) ts
      ts       = jtxns j

-- #ifdef TESTS
test_journalDateSpan = do
 "journalDateSpan" ~: do
  assertEqual "" (DateSpan (Just $ fromGregorian 2014 1 10) (Just $ fromGregorian 2014 10 11))
                 (journalDateSpan True j)
  where
    j = nulljournal{jtxns = [nulltransaction{tdate = parsedate "2014/02/01"
                                            ,tpostings = [posting{pdate=Just (parsedate "2014/01/10")}]
                                            }
                            ,nulltransaction{tdate = parsedate "2014/09/01"
                                            ,tpostings = [posting{pdate2=Just (parsedate "2014/10/10")}]
                                            }
                            ]}
-- #endif

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

-- A sample journal for testing, similar to data/sample.journal:
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
-- 2008/12/31 * pay off
--     liabilities:debts  $1
--     assets:bank:checking
--
Right samplejournal = journalBalanceTransactions $ 
         nulljournal
         {jtxns = [
           txnTieKnot $ Transaction {
             tdate=parsedate "2008/01/01",
             tdate2=Nothing,
             tstatus=False,
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
             tdate=parsedate "2008/06/01",
             tdate2=Nothing,
             tstatus=False,
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
             tdate=parsedate "2008/06/02",
             tdate2=Nothing,
             tstatus=False,
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
             tdate=parsedate "2008/06/03",
             tdate2=Nothing,
             tstatus=True,
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
             tdate=parsedate "2008/12/31",
             tdate2=Nothing,
             tstatus=False,
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

tests_Hledger_Data_Journal = TestList $
 [
  test_journalDateSpan
  -- "query standard account types" ~:
  --  do
  --   let j = journal1
  --   journalBalanceSheetAccountNames j `is` ["assets","assets:a","equity","equity:q","equity:q:qq","liabilities","liabilities:l"]
  --   journalProfitAndLossAccountNames j `is` ["expenses","expenses:e","income","income:i"]
 ]
