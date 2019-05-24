{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
{-|

Account balance history report.

-}
-- XXX not used

module Hledger.Reports.BalanceHistoryReport (
  accountBalanceHistory
)
where

import Data.Time.Calendar

import Hledger.Data
import Hledger.Query
import Hledger.Reports.ReportOptions
import Hledger.Reports.TransactionsReport


-- | Get the historical running inclusive balance of a particular account,
-- from earliest to latest posting date.
accountBalanceHistory :: ReportOpts -> Journal -> Account -> [(Day, MixedAmount)]
accountBalanceHistory ropts j a = [(getdate t, bal) | (t,_,_,_,_,bal) <- items]
  where
    (_,items) = transactionsReport ropts j acctquery
    inclusivebal = True
    acctquery = Acct $ (if inclusivebal then accountNameToAccountRegex else accountNameToAccountOnlyRegex) $ aname a
    getdate = if date2_ ropts then transactionDate2 else tdate

