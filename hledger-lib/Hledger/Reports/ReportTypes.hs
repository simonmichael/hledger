{- |
New common report types, used by the BudgetReport for now, perhaps all reports later.
-}

module Hledger.Reports.ReportTypes
where

import Data.Decimal
import Hledger.Data

type Percentage = Decimal

type Change  = MixedAmount  -- ^ A change in balance during a certain period.
type Balance = MixedAmount  -- ^ An ending balance as of some date.
type Total   = MixedAmount  -- ^ The sum of 'Change's in a report or a report row. Does not make sense for 'Balance's.
type Average = MixedAmount  -- ^ The average of 'Change's or 'Balance's in a report or report row.

-- | A generic tabular report of some value, where each row corresponds to an account
-- and each column is a date period. The column periods are usually consecutive subperiods
-- formed by splitting the overall report period by some report interval (daily, weekly, etc.)
-- Depending on the value type, this can be a report of balance changes, ending balances,
-- budget performance, etc. Successor to MultiBalanceReport.
data PeriodicReport a =
  PeriodicReport
    ( [DateSpan]            -- ^ The subperiods formed by spliting the overall report period by the report interval.
                            --   For ending-balance reports, only the end date is significant.
                            --   Usually displayed as report columns.
    , [PeriodicReportRow a] -- ^ One row per account in the report.
    , PeriodicReportRow a   -- ^ The grand totals row. The account name in this row is always empty.
    )
   deriving (Show)

type PeriodicReportRow a =
  ( AccountName  -- ^ A full account name.
  , AccountName  -- ^ Shortened form of the account name to display in tree mode. Usually the leaf name, possibly with parent accounts prefixed.
  , Int          -- ^ Indent level for displaying this account name in tree mode. 0, 1, 2... 
  , [a]          -- ^ The data value for each subperiod.
  , a            -- ^ The total of this row's values.
  , a            -- ^ The average of this row's values.
  )
