{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
{-|

Generate several common kinds of report from a journal, as \"*Report\" -
simple intermediate data structures intended to be easily rendered as
text, html, json, csv etc. by hledger commands, hamlet templates,
javascript, or whatever.

-}

module Hledger.Reports (
  module Hledger.Reports.ReportOptions,
  module Hledger.Reports.ReportTypes,
  module Hledger.Reports.EntriesReport,
  module Hledger.Reports.PostingsReport,
  module Hledger.Reports.TransactionsReports,
  module Hledger.Reports.BalanceReport,
  module Hledger.Reports.MultiBalanceReports,
--   module Hledger.Reports.BalanceHistoryReport,

  -- * Tests
  tests_Hledger_Reports
)
where

import Test.HUnit

import Hledger.Reports.ReportOptions
import Hledger.Reports.ReportTypes
import Hledger.Reports.EntriesReport
import Hledger.Reports.PostingsReport
import Hledger.Reports.TransactionsReports
import Hledger.Reports.BalanceReport
import Hledger.Reports.MultiBalanceReports
-- import Hledger.Reports.BalanceHistoryReport

tests_Hledger_Reports :: Test
tests_Hledger_Reports = TestList $
 -- ++ tests_isInterestingIndented
 [
 tests_Hledger_Reports_ReportOptions,
 tests_Hledger_Reports_EntriesReport,
 tests_Hledger_Reports_PostingsReport,
 tests_Hledger_Reports_BalanceReport,
 tests_Hledger_Reports_MultiBalanceReport
 ]
