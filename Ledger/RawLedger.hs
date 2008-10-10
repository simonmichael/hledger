{-|

A 'RawLedger' is a parsed ledger file. We call it raw to distinguish from
the cached 'Ledger'.

-}

module Ledger.RawLedger
where
import Ledger.Utils
import Ledger.Types
import Ledger.AccountName
import Ledger.Entry
import Ledger.Transaction


rawledgertests = TestList [
                 ]

instance Show RawLedger where
    show l = printf "RawLedger with %d entries, %d accounts: %s"
             ((length $ entries l) +
              (length $ modifier_entries l) +
              (length $ periodic_entries l))
             (length accounts)
             (show accounts)
             where accounts = flatten $ rawLedgerAccountNameTree l

rawLedgerTransactions :: RawLedger -> [Transaction]
rawLedgerTransactions = txns . entries
    where
      txns :: [Entry] -> [Transaction]
      txns es = concat $ map flattenEntry $ zip es (iterate (+1) 1)

rawLedgerAccountNamesUsed :: RawLedger -> [AccountName]
rawLedgerAccountNamesUsed = accountNamesFromTransactions . rawLedgerTransactions

rawLedgerAccountNames :: RawLedger -> [AccountName]
rawLedgerAccountNames = sort . expandAccountNames . rawLedgerAccountNamesUsed

rawLedgerAccountNameTree :: RawLedger -> Tree AccountName
rawLedgerAccountNameTree l = accountNameTreeFrom $ rawLedgerAccountNames l

-- | Remove ledger entries we are not interested in.
-- Keep only those which fall between the begin and end dates, and match
-- the description pattern.
filterRawLedgerEntries :: String -> String -> Regex -> RawLedger -> RawLedger
filterRawLedgerEntries begin end descpat = 
    filterRawLedgerEntriesByDate begin end .
    filterRawLedgerEntriesByDescription descpat

-- | Keep only entries whose description matches the description pattern.
filterRawLedgerEntriesByDescription :: Regex -> RawLedger -> RawLedger
filterRawLedgerEntriesByDescription descpat (RawLedger ms ps es f) = 
    RawLedger ms ps (filter matchdesc es) f
    where
      matchdesc :: Entry -> Bool
      matchdesc e = case matchRegex descpat (edescription e) of
                      Nothing -> False
                      otherwise -> True

-- | Keep only entries which fall between begin and end dates. 
-- We include entries on the begin date and exclude entries on the end
-- date, like ledger.  An empty date string means no restriction.
filterRawLedgerEntriesByDate :: String -> String -> RawLedger -> RawLedger
filterRawLedgerEntriesByDate begin end (RawLedger ms ps es f) = 
    RawLedger ms ps (filter matchdate es) f
    where
      matchdate :: Entry -> Bool
      matchdate e = (begin == "" || entrydate >= begindate) && 
                    (end == "" || entrydate < enddate)
                    where 
                      begindate = parsedate begin :: UTCTime
                      enddate   = parsedate end
                      entrydate = parsedate $ edate e

