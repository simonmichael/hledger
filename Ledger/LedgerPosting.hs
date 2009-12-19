{-|

A 'LedgerPosting' is a 'Posting' with its parent 'Transaction' \'s date
and description attached. We flatten Transactions into multiple
LedgerPostings, which most of the time are simpler to work with. (So far,
at least - this is not quite settled.)

-}

module Ledger.LedgerPosting
where
import Ledger.Dates
import Ledger.Utils
import Ledger.Types
import Ledger.Transaction (showAccountName)
import Ledger.Amount


instance Show LedgerPosting where show=showLedgerPosting

showLedgerPosting :: LedgerPosting -> String
showLedgerPosting (LedgerPosting _ stat d desc a amt lptype) = 
    s ++ unwords [showDate d,desc,a',show amt,show lptype]
    where s = if stat then " *" else ""
          a' = showAccountName Nothing lptype a

-- | Convert a 'Transaction' to two or more 'LedgerPosting's. An id number
-- is attached to the transactions to preserve their grouping - it should
-- be unique per entry.
flattenTransaction :: (Transaction, Int) -> [LedgerPosting]
flattenTransaction (Transaction d _ s _ desc _ ps _, n) = 
    [LedgerPosting n s d desc (paccount p) (pamount p) (ptype p) | p <- ps]

accountNamesFromLedgerPostings :: [LedgerPosting] -> [AccountName]
accountNamesFromLedgerPostings = nub . map lpaccount

sumLedgerPostings :: [LedgerPosting] -> MixedAmount
sumLedgerPostings = sum . map lpamount

nullledgerposting :: LedgerPosting
nullledgerposting = LedgerPosting 0 False (parsedate "1900/1/1") "" "" nullmixedamt RegularPosting

-- | Does the given transaction fall within the given date span ?
isLedgerPostingInDateSpan :: DateSpan -> LedgerPosting -> Bool
isLedgerPostingInDateSpan (DateSpan Nothing Nothing)   _ = True
isLedgerPostingInDateSpan (DateSpan Nothing (Just e))  (LedgerPosting{lpdate=d}) = d<e
isLedgerPostingInDateSpan (DateSpan (Just b) Nothing)  (LedgerPosting{lpdate=d}) = d>=b
isLedgerPostingInDateSpan (DateSpan (Just b) (Just e)) (LedgerPosting{lpdate=d}) = d>=b && d<e

