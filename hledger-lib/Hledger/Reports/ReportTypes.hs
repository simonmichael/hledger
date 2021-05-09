{- |
New common report types, used by the BudgetReport for now, perhaps all reports later.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}

module Hledger.Reports.ReportTypes
( PeriodicReport(..)
, PeriodicReportRow(..)

, Percentage
, Change
, Balance
, Total
, Average

, periodicReportSpan
, prMapName
, prMapMaybeName

, CompoundPeriodicReport(..)
, CBCSubreportSpec(..)

, DisplayName(..)
, flatDisplayName
, treeDisplayName

, prrFullName
, prrDisplayName
, prrDepth
) where

import Data.Aeson (ToJSON(..))
import Data.Decimal (Decimal)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

import Hledger.Data
import Hledger.Query (Query)
import Hledger.Reports.ReportOptions (ReportOpts)

type Percentage = Decimal

type Change  = MixedAmount  -- ^ A change in balance during a certain period.
type Balance = MixedAmount  -- ^ An ending balance as of some date.
type Total   = MixedAmount  -- ^ The sum of 'Change's in a report or a report row. Does not make sense for 'Balance's.
type Average = MixedAmount  -- ^ The average of 'Change's or 'Balance's in a report or report row.

-- | A periodic report is a generic tabular report, where each row corresponds
-- to some label (usually an account name) and each column to a date period.
-- The column periods are usually consecutive subperiods formed by splitting
-- the overall report period by some report interval (daily, weekly, etc.).
-- It has:
--
-- 1. a list of each column's period (date span)
--
-- 2. a list of rows, each containing:
--
--   * an account label
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

data PeriodicReport a b =
  PeriodicReport
  { prDates  :: [DateSpan]               -- The subperiods formed by splitting the overall
                                         -- report period by the report interval. For
                                         -- ending-balance reports, only the end date is
                                         -- significant. Usually displayed as report columns.
  , prRows   :: [PeriodicReportRow a b]  -- One row per account in the report.
  , prTotals :: PeriodicReportRow () b   -- The grand totals row.
  } deriving (Show, Functor, Generic, ToJSON)

data PeriodicReportRow a b =
  PeriodicReportRow
  { prrName    :: a    -- An account name.
  , prrAmounts :: [b]  -- The data value for each subperiod.
  , prrTotal   :: b    -- The total of this row's values.
  , prrAverage :: b    -- The average of this row's values.
  } deriving (Show, Functor, Generic, ToJSON)

instance Semigroup b => Semigroup (PeriodicReportRow a b) where
  (PeriodicReportRow _ amts1 t1 a1) <> (PeriodicReportRow n2 amts2 t2 a2) =
      PeriodicReportRow n2 (sumPadded amts1 amts2) (t1 <> t2) (a1 <> a2)
    where
      sumPadded (a:as) (b:bs) = (a <> b) : sumPadded as bs
      sumPadded as     []     = as
      sumPadded []     bs     = bs

-- | Figure out the overall date span of a PeriodicReport
periodicReportSpan :: PeriodicReport a b -> DateSpan
periodicReportSpan (PeriodicReport [] _ _)       = DateSpan Nothing Nothing
periodicReportSpan (PeriodicReport colspans _ _) = DateSpan (spanStart $ head colspans) (spanEnd $ last colspans)

-- | Map a function over the row names.
prMapName :: (a -> b) -> PeriodicReport a c -> PeriodicReport b c
prMapName f report = report{prRows = map (prrMapName f) $ prRows report}

-- | Map a function over the row names, possibly discarding some.
prMapMaybeName :: (a -> Maybe b) -> PeriodicReport a c -> PeriodicReport b c
prMapMaybeName f report = report{prRows = mapMaybe (prrMapMaybeName f) $ prRows report}

-- | Map a function over the row names of the PeriodicReportRow.
prrMapName :: (a -> b) -> PeriodicReportRow a c -> PeriodicReportRow b c
prrMapName f row = row{prrName = f $ prrName row}

-- | Map maybe a function over the row names of the PeriodicReportRow.
prrMapMaybeName :: (a -> Maybe b) -> PeriodicReportRow a c -> Maybe (PeriodicReportRow b c)
prrMapMaybeName f row = case f $ prrName row of
    Nothing -> Nothing
    Just a  -> Just row{prrName = a}


-- | A compound balance report has:
--
-- * an overall title
--
-- * the period (date span) of each column
--
-- * one or more named, normal-positive multi balance reports,
--   with columns corresponding to the above, and a flag indicating
--   whether they increased or decreased the overall totals
--
-- * a list of overall totals for each column, and their grand total and average
--
-- It is used in compound balance report commands like balancesheet,
-- cashflow and incomestatement.
data CompoundPeriodicReport a b = CompoundPeriodicReport
  { cbrTitle      :: Text
  , cbrDates      :: [DateSpan]
  , cbrSubreports :: [(Text, PeriodicReport a b, Bool)]
  , cbrTotals     :: PeriodicReportRow () b
  } deriving (Show, Functor, Generic, ToJSON)

-- | Description of one subreport within a compound balance report.
-- Part of a "CompoundBalanceCommandSpec", but also used in hledger-lib.
data CBCSubreportSpec a = CBCSubreportSpec
  { cbcsubreporttitle          :: Text                      -- ^ The title to use for the subreport
  , cbcsubreportquery          :: Journal -> Query          -- ^ The Query to use for the subreport
  , cbcsubreportoptions        :: ReportOpts -> ReportOpts  -- ^ A function to transform the ReportOpts used to produce the subreport
  , cbcsubreporttransform      :: PeriodicReport DisplayName MixedAmount -> PeriodicReport a MixedAmount  -- ^ A function to transform the result of the subreport
  , cbcsubreportincreasestotal :: Bool                      -- ^ Whether the subreport and overall report total are of the same sign (e.g. Assets are normally
                                                            --   positive in a balance sheet report, as is the overall total. Liabilities are normally of the
                                                            --   opposite sign.)
  }


-- | A full name, display name, and depth for an account.
data DisplayName = DisplayName
    { displayFull :: AccountName
    , displayName :: AccountName
    , displayDepth :: Int
    } deriving (Show, Eq, Ord)

instance ToJSON DisplayName where
    toJSON = toJSON . displayFull
    toEncoding = toEncoding . displayFull

-- | Construct a flat display name, where the full name is also displayed at
-- depth 1
flatDisplayName :: AccountName -> DisplayName
flatDisplayName a = DisplayName a a 1

-- | Construct a tree display name, where only the leaf is displayed at its
-- given depth
treeDisplayName :: AccountName -> DisplayName
treeDisplayName a = DisplayName a (accountLeafName a) (accountNameLevel a)

-- | Get the full, canonical, name of a PeriodicReportRow tagged by a
-- DisplayName.
prrFullName :: PeriodicReportRow DisplayName a -> AccountName
prrFullName = displayFull . prrName

-- | Get the display name of a PeriodicReportRow tagged by a DisplayName.
prrDisplayName :: PeriodicReportRow DisplayName a -> AccountName
prrDisplayName = displayName . prrName

-- | Get the display depth of a PeriodicReportRow tagged by a DisplayName.
prrDepth :: PeriodicReportRow DisplayName a -> Int
prrDepth = displayDepth . prrName
