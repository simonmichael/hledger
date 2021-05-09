{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
  module Hledger.Reports.TransactionsReport,
  module Hledger.Reports.AccountTransactionsReport,
  module Hledger.Reports.BalanceReport,
  module Hledger.Reports.MultiBalanceReport,
  module Hledger.Reports.BudgetReport,
  -- * Tests
  tests_Reports
)
where

import Hledger.Reports.ReportOptions
import Hledger.Reports.ReportTypes
import Hledger.Reports.AccountTransactionsReport
import Hledger.Reports.EntriesReport
import Hledger.Reports.PostingsReport
import Hledger.Reports.TransactionsReport
import Hledger.Reports.BalanceReport
import Hledger.Reports.MultiBalanceReport
import Hledger.Reports.BudgetReport
import Hledger.Utils.Test

tests_Reports = tests "Reports" [
   tests_BalanceReport
  ,tests_BudgetReport
  ,tests_AccountTransactionsReport
  ,tests_EntriesReport
  ,tests_MultiBalanceReport
  ,tests_PostingsReport
  ]
