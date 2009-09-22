{-|

A compound data type for efficiency. A 'Transaction' is a 'Posting' with
its parent 'LedgerTransaction' \'s date and description attached. The
\"transaction\" term is pretty ingrained in the code, docs and with users,
so we've kept it. These are what we work with most of the time when doing
reports.

-}

module Ledger.Transaction
where
import Ledger.Dates
import Ledger.Utils
import Ledger.Types
import Ledger.LedgerTransaction (showAccountName)
import Ledger.Amount


instance Show Transaction where show=showTransaction

showTransaction :: Transaction -> String
showTransaction (Transaction _ stat d desc a amt ttype) = 
    s ++ unwords [showDate d,desc,a',show amt,show ttype]
    where s = if stat then " *" else ""
          a' = showAccountName Nothing ttype a

-- | Convert a 'LedgerTransaction' to two or more 'Transaction's. An id number
-- is attached to the transactions to preserve their grouping - it should
-- be unique per entry.
flattenLedgerTransaction :: (LedgerTransaction, Int) -> [Transaction]
flattenLedgerTransaction (LedgerTransaction d _ s _ desc _ ps _, n) = 
    [Transaction n s d desc (paccount p) (pamount p) (ptype p) | p <- ps]

accountNamesFromTransactions :: [Transaction] -> [AccountName]
accountNamesFromTransactions = nub . map taccount

sumTransactions :: [Transaction] -> MixedAmount
sumTransactions = sum . map tamount

nulltxn :: Transaction
nulltxn = Transaction 0 False (parsedate "1900/1/1") "" "" nullmixedamt RegularPosting

-- | Does the given transaction fall within the given date span ?
isTransactionInDateSpan :: DateSpan -> Transaction -> Bool
isTransactionInDateSpan (DateSpan Nothing Nothing)   _ = True
isTransactionInDateSpan (DateSpan Nothing (Just e))  (Transaction{tdate=d}) = d<e
isTransactionInDateSpan (DateSpan (Just b) Nothing)  (Transaction{tdate=d}) = d>=b
isTransactionInDateSpan (DateSpan (Just b) (Just e)) (Transaction{tdate=d}) = d>=b && d<e

