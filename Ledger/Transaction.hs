{-|

A 'Transaction' is a 'RawTransaction' with its parent 'Entry' \'s date and
description attached. These are what we actually query when doing reports.

-}

module Ledger.Transaction
where
import Ledger.Utils
import Ledger.Types
import Ledger.Entry
import Ledger.RawTransaction
import Ledger.Amount


instance Show Transaction where 
    show (Transaction eno d desc a amt) = 
        unwords [d,desc,a,show amt]

-- | Convert a 'Entry' to two or more 'Transaction's. An id number
-- is attached to the transactions to preserve their grouping - it should
-- be unique per entry.
flattenEntry :: (Entry, Int) -> [Transaction]
flattenEntry (Entry d _ _ desc _ ts _, e) = 
    [Transaction e d desc (taccount t) (tamount t) | t <- ts]

accountNamesFromTransactions :: [Transaction] -> [AccountName]
accountNamesFromTransactions ts = nub $ map account ts

sumTransactions :: [Transaction] -> Amount
sumTransactions = sum . map amount

nulltxn = Transaction 0 "" "" "" nullamt
