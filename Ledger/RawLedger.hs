{-|

A 'RawLedger' is a parsed ledger file. We call it raw to distinguish from
the cached 'Ledger'.

-}

module Ledger.RawLedger
where
import qualified Data.Map as Map
import Data.Map ((!))
import Ledger.Utils
import Ledger.Types
import Ledger.AccountName
import Ledger.Amount
import Ledger.Entry
import Ledger.Transaction
import Ledger.RawTransaction
import Ledger.TimeLog


instance Show RawLedger where
    show l = printf "RawLedger with %d entries, %d accounts: %s"
             ((length $ entries l) +
              (length $ modifier_entries l) +
              (length $ periodic_entries l))
             (length accounts)
             (show accounts)
             -- ++ (show $ rawLedgerTransactions l)
             where accounts = flatten $ rawLedgerAccountNameTree l

rawLedgerEmpty :: RawLedger
rawLedgerEmpty = RawLedger { modifier_entries = []
                           , periodic_entries = []
                           , entries = []
                           , open_timelog_entries = []
                           , historical_prices = []
                           , final_comment_lines = []
                           }

addEntry :: Entry -> RawLedger -> RawLedger
addEntry e l0 = l0 { entries = e : (entries l0) }

addModifierEntry :: ModifierEntry -> RawLedger -> RawLedger
addModifierEntry me l0 = l0 { modifier_entries = me : (modifier_entries l0) }

addPeriodicEntry :: PeriodicEntry -> RawLedger -> RawLedger
addPeriodicEntry pe l0 = l0 { periodic_entries = pe : (periodic_entries l0) }

addHistoricalPrice :: HistoricalPrice -> RawLedger -> RawLedger
addHistoricalPrice h l0 = l0 { historical_prices = h : (historical_prices l0) }

addTimeLogEntry :: TimeLogEntry -> RawLedger -> RawLedger
addTimeLogEntry tle l0 = l0 { open_timelog_entries = tle : (open_timelog_entries l0) }

rawLedgerTransactions :: RawLedger -> [Transaction]
rawLedgerTransactions = txnsof . entries
    where txnsof es = concat $ map flattenEntry $ zip es [1..]

rawLedgerAccountNamesUsed :: RawLedger -> [AccountName]
rawLedgerAccountNamesUsed = accountNamesFromTransactions . rawLedgerTransactions

rawLedgerAccountNames :: RawLedger -> [AccountName]
rawLedgerAccountNames = sort . expandAccountNames . rawLedgerAccountNamesUsed

rawLedgerAccountNameTree :: RawLedger -> Tree AccountName
rawLedgerAccountNameTree l = accountNameTreeFrom $ rawLedgerAccountNames l

-- | Remove ledger entries we are not interested in.
-- Keep only those which fall between the begin and end dates, and match
-- the description pattern, and are cleared or real if those options are active.
filterRawLedger :: DateSpan -> [String] -> Bool -> Bool -> RawLedger -> RawLedger
filterRawLedger span pats clearedonly realonly = 
    filterRawLedgerTransactionsByRealness realonly .
    filterRawLedgerEntriesByClearedStatus clearedonly .
    filterRawLedgerEntriesByDate span .
    filterRawLedgerEntriesByDescription pats

-- | Keep only entries whose description matches the description patterns.
filterRawLedgerEntriesByDescription :: [String] -> RawLedger -> RawLedger
filterRawLedgerEntriesByDescription pats (RawLedger ms ps es tls hs f) = 
    RawLedger ms ps (filter matchdesc es) tls hs f
    where matchdesc = matchpats pats . edescription

-- | Keep only entries which fall between begin and end dates. 
-- We include entries on the begin date and exclude entries on the end
-- date, like ledger.  An empty date string means no restriction.
filterRawLedgerEntriesByDate :: DateSpan -> RawLedger -> RawLedger
filterRawLedgerEntriesByDate (DateSpan begin end) (RawLedger ms ps es tls hs f) = 
    RawLedger ms ps (filter matchdate es) tls hs f
    where 
      matchdate e = (maybe True (edate e>=) begin) && (maybe True (edate e<) end)

-- | Keep only entries with cleared status, if the flag is true, otherwise
-- do no filtering.
filterRawLedgerEntriesByClearedStatus :: Bool -> RawLedger -> RawLedger
filterRawLedgerEntriesByClearedStatus False l = l
filterRawLedgerEntriesByClearedStatus True  (RawLedger ms ps es tls hs f) =
    RawLedger ms ps (filter estatus es) tls hs f

-- | Strip out any virtual transactions, if the flag is true, otherwise do
-- no filtering.
filterRawLedgerTransactionsByRealness :: Bool -> RawLedger -> RawLedger
filterRawLedgerTransactionsByRealness False l = l
filterRawLedgerTransactionsByRealness True (RawLedger ms ps es tls hs f) =
    RawLedger ms ps (map filtertxns es) tls hs f
    where filtertxns e@Entry{etransactions=ts} = e{etransactions=filter isReal ts}

-- | Strip out any transactions to accounts deeper than the specified depth
-- (and any entries which have no transactions as a result).
filterRawLedgerTransactionsByDepth :: Int -> RawLedger -> RawLedger
filterRawLedgerTransactionsByDepth depth (RawLedger ms ps es tls hs f) =
    RawLedger ms ps (filter (not . null . etransactions) $ map filtertxns es) tls hs f
    where filtertxns e@Entry{etransactions=ts} = 
              e{etransactions=filter ((<= depth) . accountNameLevel . taccount) ts}

-- | Keep only entries which affect accounts matched by the account patterns.
filterRawLedgerEntriesByAccount :: [String] -> RawLedger -> RawLedger
filterRawLedgerEntriesByAccount apats (RawLedger ms ps es tls hs f) =
    RawLedger ms ps (filter (any (matchpats apats . taccount) . etransactions) es) tls hs f

-- | Give all a ledger's amounts their canonical display settings.  That
-- is, in each commodity, amounts will use the display settings of the
-- first amount detected, and the greatest precision of the amounts
-- detected. Also, amounts are converted to cost basis if that flag is
-- active.
canonicaliseAmounts :: Bool -> RawLedger -> RawLedger
canonicaliseAmounts costbasis l@(RawLedger ms ps es tls hs f) = RawLedger ms ps (map fixentry es) tls hs f
    where 
      fixentry (Entry d s c de co ts pr) = Entry d s c de co (map fixrawtransaction ts) pr
      fixrawtransaction (RawTransaction ac a c t) = RawTransaction ac (fixmixedamount a) c t
      fixmixedamount (Mixed as) = Mixed $ map fixamount as
      fixamount = fixcommodity . (if costbasis then costOfAmount else id)
      fixcommodity a = a{commodity=c} where c = canonicalcommoditymap ! (symbol $ commodity a)
      canonicalcommoditymap = 
          Map.fromList [(s,firstc{precision=maxp}) | s <- commoditysymbols,
                        let cs = commoditymap ! s,
                        let firstc = head cs,
                        let maxp = maximum $ map precision cs
                       ]
      commoditymap = Map.fromList [(s,commoditieswithsymbol s) | s <- commoditysymbols]
      commoditieswithsymbol s = filter ((s==) . symbol) commodities
      commoditysymbols = nub $ map symbol commodities
      commodities = map commodity $ concatMap (amounts . amount) $ rawLedgerTransactions l

-- | Get just the amounts from a ledger, in the order parsed.
rawLedgerAmounts :: RawLedger -> [MixedAmount]
rawLedgerAmounts = map amount . rawLedgerTransactions

-- | Get just the ammount commodities from a ledger, in the order parsed.
rawLedgerCommodities :: RawLedger -> [Commodity]
rawLedgerCommodities = map commodity . concatMap amounts . rawLedgerAmounts

-- | Get just the amount precisions from a ledger, in the order parsed.
rawLedgerPrecisions :: RawLedger -> [Int]
rawLedgerPrecisions = map precision . rawLedgerCommodities

-- | Close any open timelog sessions using the provided current time.
rawLedgerConvertTimeLog :: LocalTime -> RawLedger -> RawLedger
rawLedgerConvertTimeLog t l0 = l0 { entries = convertedTimeLog ++ entries l0
                                  , open_timelog_entries = []
                                  }
    where convertedTimeLog = entriesFromTimeLogEntries t $ open_timelog_entries l0

