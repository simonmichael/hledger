{-|

A 'RawLedger' is a parsed ledger file. We call it raw to distinguish from
the cached 'Ledger'.

-}

module Ledger.RawLedger
where
import qualified Data.Map as Map
import Ledger.Utils
import Ledger.Types
import Ledger.AccountName
import Ledger.Amount
import Ledger.Entry
import Ledger.Transaction
import Ledger.RawTransaction


instance Show RawLedger where
    show l = printf "RawLedger with %d entries, %d accounts: %s"
             ((length $ entries l) +
              (length $ modifier_entries l) +
              (length $ periodic_entries l))
             (length accounts)
             (show accounts)
             -- ++ (show $ rawLedgerTransactions l)
             where accounts = flatten $ rawLedgerAccountNameTree l

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
filterRawLedger :: Maybe Day -> Maybe Day -> [String] -> Bool -> Bool -> RawLedger -> RawLedger
filterRawLedger begin end pats clearedonly realonly = 
    filterRawLedgerTransactionsByRealness realonly .
    filterRawLedgerEntriesByClearedStatus clearedonly .
    filterRawLedgerEntriesByDate begin end .
    filterRawLedgerEntriesByDescription pats

-- | Keep only entries whose description matches the description patterns.
filterRawLedgerEntriesByDescription :: [String] -> RawLedger -> RawLedger
filterRawLedgerEntriesByDescription pats (RawLedger ms ps es f) = 
    RawLedger ms ps (filter matchdesc es) f
    where matchdesc = matchpats pats . edescription

-- | Keep only entries which fall between begin and end dates. 
-- We include entries on the begin date and exclude entries on the end
-- date, like ledger.  An empty date string means no restriction.
filterRawLedgerEntriesByDate :: Maybe Day -> Maybe Day -> RawLedger -> RawLedger
filterRawLedgerEntriesByDate begin end (RawLedger ms ps es f) = 
    RawLedger ms ps (filter matchdate es) f
    where 
      matchdate e = (maybe True (edate e>=) begin) && (maybe True (edate e<) end)

-- | Keep only entries with cleared status, if the flag is true, otherwise
-- do no filtering.
filterRawLedgerEntriesByClearedStatus :: Bool -> RawLedger -> RawLedger
filterRawLedgerEntriesByClearedStatus False l = l
filterRawLedgerEntriesByClearedStatus True  (RawLedger ms ps es f) =
    RawLedger ms ps (filter estatus es) f

-- | Strip out any virtual transactions, if the flag is true, otherwise do
-- no filtering.
filterRawLedgerTransactionsByRealness :: Bool -> RawLedger -> RawLedger
filterRawLedgerTransactionsByRealness False l = l
filterRawLedgerTransactionsByRealness True (RawLedger ms ps es f) =
    RawLedger ms ps (map filtertxns es) f
    where filtertxns e@Entry{etransactions=ts} = e{etransactions=filter isReal ts}

-- | Keep only entries which affect accounts matched by the account patterns.
filterRawLedgerEntriesByAccount :: [String] -> RawLedger -> RawLedger
filterRawLedgerEntriesByAccount apats (RawLedger ms ps es f) =
    RawLedger ms ps (filter (any (matchpats apats . taccount) . etransactions) es) f

-- | Give all a ledger's amounts their canonical display settings.  That
-- is, in each commodity, amounts will use the display settings of the
-- first amount detected, and the greatest precision of the amounts
-- detected. Also, amounts are converted to cost basis if that flag is
-- active.
canonicaliseAmounts :: Bool -> RawLedger -> RawLedger
canonicaliseAmounts costbasis l@(RawLedger ms ps es f) = RawLedger ms ps (map fixEntryAmounts es) f
    where 
      fixEntryAmounts (Entry d s c de co ts pr) = Entry d s c de co (map fixRawTransactionAmounts ts) pr
      fixRawTransactionAmounts (RawTransaction ac a c t) = RawTransaction ac (fixMixedAmount a) c t
      fixMixedAmount (Mixed as) = Mixed $ map fixAmount as
      fixAmount | costbasis = fixcommodity . costOfAmount
                | otherwise = fixcommodity
      fixcommodity a = a{commodity=canonicalcommodity $ commodity a}
      canonicalcommodity c = (firstoccurrenceof c){precision=maxprecision c}
          where
            firstoccurrenceof c = head $ rawLedgerCommoditiesWithSymbol l (symbol c)
            maxprecision c = maximum $ map precision $ rawLedgerCommoditiesWithSymbol l (symbol c)

-- | Get all amount commodities with a given symbol, in the order parsed.
-- Must be called with a good symbol or it will fail.
rawLedgerCommoditiesWithSymbol :: RawLedger -> String -> [Commodity]
rawLedgerCommoditiesWithSymbol l s = 
    fromMaybe (error $ "no such commodity "++s) (Map.lookup s map)
    where
      map = Map.fromList [(symbol $ head cs,cs) | cs <- groupBy same $ rawLedgerCommodities l]
      same c1 c2 = symbol c1 == symbol c2

-- | Get just the ammount commodities from a ledger, in the order parsed.
rawLedgerCommodities :: RawLedger -> [Commodity]
rawLedgerCommodities = map commodity . concatMap amounts . rawLedgerAmounts

-- | Get just the amounts from a ledger, in the order parsed.
rawLedgerAmounts :: RawLedger -> [MixedAmount]
rawLedgerAmounts = map amount . rawLedgerTransactions

-- | Get just the amount precisions from a ledger, in the order parsed.
rawLedgerPrecisions :: RawLedger -> [Int]
rawLedgerPrecisions = map precision . rawLedgerCommodities

