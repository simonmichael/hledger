{- |
New common report types, used by the BudgetReport for now, perhaps all reports later.
-}

module Hledger.Reports.ReportTypes
( PeriodicReport(..)
, PeriodicReportRow(..)

, Percentage
, Change
, Balance
, Total
, Average

, periodicReportSpan
, prNegate
, prNormaliseSign
) where

import Data.Decimal
import Hledger.Data

type Percentage = Decimal

type Change  = MixedAmount  -- ^ A change in balance during a certain period.
type Balance = MixedAmount  -- ^ An ending balance as of some date.
type Total   = MixedAmount  -- ^ The sum of 'Change's in a report or a report row. Does not make sense for 'Balance's.
type Average = MixedAmount  -- ^ The average of 'Change's or 'Balance's in a report or report row.

-- | A periodic report is a generic tabular report, where each row corresponds
-- to an account and each column to a date period. The column periods are
-- usually consecutive subperiods formed by splitting the overall report period
-- by some report interval (daily, weekly, etc.). It has:
--
-- 1. a list of each column's period (date span)
--
-- 2. a list of rows, each containing:
--
--   * the full account name
--
--   * the leaf account name
--
--   * the account's depth
--
--   * A list of amounts, one for each column. Depending on the value type,
--     these can represent balance changes, ending balances, budget
--     performance, etc. (for example, see 'BalanceType' and
--     "Hledger.Cli.Commands.Balance").
--
--   * the total of the row's amounts for a periodic report,
--     or zero for cumulative/historical reports (since summing
--     end balances generally doesn't make sense).
--
--   * the average of the row's amounts
--
-- 3. the column totals, and the overall grand total (or zero for
-- cumulative/historical reports) and grand average.

data PeriodicReport a =
  PeriodicReport
  { prDates  :: [DateSpan]             -- The subperiods formed by splitting the overall
                                       -- report period by the report interval. For
                                       -- ending-balance reports, only the end date is
                                       -- significant. Usually displayed as report columns.
  , prRows   :: [PeriodicReportRow a]  -- One row per account in the report.
  , prTotals :: PeriodicReportRow a    -- The grand totals row. The account name in this row is always empty.
  } deriving (Show)

data PeriodicReportRow a =
  PeriodicReportRow
  { prrName    :: AccountName  -- A full account name.
  , prrLeaf    :: AccountName  -- Shortened form of the account name to display
                               -- in tree mode. Usually the leaf name, possibly
                               -- with parent accounts prefixed.
  , prrDepth   :: Int          -- Indent level for displaying this account name in tree mode. 0, 1, 2...
  , prrAmounts :: [a]          -- The data value for each subperiod.
  , prrTotal   :: a            -- The total of this row's values.
  , prrAverage :: a            -- The average of this row's values.
  } deriving (Show)

-- | Figure out the overall date span of a PeridicReport
periodicReportSpan :: PeriodicReport a -> DateSpan
periodicReportSpan (PeriodicReport [] _ _)       = DateSpan Nothing Nothing
periodicReportSpan (PeriodicReport colspans _ _) = DateSpan (spanStart $ head colspans) (spanEnd $ last colspans)

-- | Given a PeriodicReport and its normal balance sign,
-- if it is known to be normally negative, convert it to normally positive.
prNormaliseSign :: Num a => NormalSign -> PeriodicReport a -> PeriodicReport a
prNormaliseSign NormallyNegative = prNegate
prNormaliseSign _ = id

-- | Flip the sign of all amounts in a PeriodicReport.
prNegate :: Num a => PeriodicReport a -> PeriodicReport a
prNegate (PeriodicReport colspans rows totalsrow) =
    PeriodicReport colspans (map rowNegate rows) (rowNegate totalsrow)
  where
    rowNegate (PeriodicReportRow name leaf indent amts tot avg) =
        PeriodicReportRow name leaf indent (map negate amts) (-tot) (-avg)
