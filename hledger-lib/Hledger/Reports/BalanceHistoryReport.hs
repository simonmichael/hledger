{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
{-|

Account balance history report.

-}

module Hledger.Reports.BalanceHistoryReport (
  accountBalanceHistory

  -- -- * Tests
  -- tests_Hledger_Reports_BalanceReport
)
where

import Data.Time.Calendar
-- import Test.HUnit

import Hledger.Data
import Hledger.Query
import Hledger.Reports.ReportOptions
import Hledger.Reports.TransactionsReports


-- | Get the historical running inclusive balance of a particular account,
-- from earliest to latest posting date.
accountBalanceHistory :: ReportOpts -> Journal -> Account -> [(Day, MixedAmount)]
accountBalanceHistory ropts j a = [(getdate t, bal) | (t,_,_,_,_,bal) <- items]
  where
    (_,items) = journalTransactionsReport ropts j acctquery
    inclusivebal = True
    acctquery = Acct $ (if inclusivebal then accountNameToAccountRegex else accountNameToAccountOnlyRegex) $ aname a
    getdate = if date2_ ropts then transactionDate2 else tdate

