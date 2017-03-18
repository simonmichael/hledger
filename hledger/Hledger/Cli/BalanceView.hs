{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-|

This module is used by the 'balancesheet', 'incomestatement', and
'cashflow' commands to print out acocunt balances based on a specific
"view", which consists of a title and multiple named queries that are
aggregated and totaled.

-}

module Hledger.Cli.BalanceView (
  BalanceView(..)
 ,balanceviewmode
 ,balanceviewReport
) where

import Control.Monad (unless)
import Data.Time.Calendar (Day)
import Data.List (intercalate)
import Data.Monoid (Sum(..), (<>))
import System.Console.CmdArgs.Explicit

import Hledger
import Hledger.Cli.Balance
import Hledger.Cli.CliOptions

-- | Describes a view for the balance, which can consist of multiple
-- separate named queries that are aggregated and totaled.
data BalanceView = BalanceView {
      bvmode     :: String,                        -- ^ command line mode of the view
      bvaliases  :: [String],                      -- ^ command line aliases
      bvhelp     :: String,                        -- ^ command line help message
      bvtitle    :: String,                        -- ^ title of the view
      bvqueries  :: [(String, Journal -> Query)],  -- ^ named queries that make up the view
      bvsnapshot :: Bool                           -- ^ whether or not the view is a snapshot,
                                                   --   ignoring begin date in reporting period
}

balanceviewmode :: BalanceView -> Mode RawOpts
balanceviewmode BalanceView{..} = (defCommandMode $ bvmode : bvaliases) {
  modeHelp = bvhelp `withAliases` bvaliases
 ,modeGroupFlags = Group {
     groupUnnamed = [
      flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show accounts as a list"
     ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
     ,flagNone ["no-total","N"] (\opts -> setboolopt "no-total" opts) "omit the final total row"
     ,flagNone ["no-elide"] (\opts -> setboolopt "no-elide" opts) "don't squash boring parent accounts (in tree mode)"
     ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "use this custom line format (in simple reports)"
     ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }

balanceviewQueryReport
    :: ReportOpts
    -> Query
    -> Journal
    -> String
    -> (Journal -> Query)
    -> ([String], Sum MixedAmount)
balanceviewQueryReport ropts q0 j t q = ([view], Sum amt)
    where
      q' = And [q0, q j]
      rep@(_ , amt) = balanceReport ropts q' j
      view = intercalate "\n" [t <> ":", balanceReportAsText ropts rep]

-- | Prints out a balance report according to a given view
balanceviewReport :: BalanceView -> CliOpts -> Journal -> IO ()
balanceviewReport BalanceView{..} CliOpts{reportopts_=ropts} j = do
  currDay   <- getCurrentDay
  let q0 | bvsnapshot = queryFromOpts currDay (withoutBeginDate ropts)
         | otherwise  = queryFromOpts currDay ropts
      (views, amt) =
        foldMap (uncurry (balanceviewQueryReport ropts q0 j))
           bvqueries
  mapM_ putStrLn (bvtitle : "" : views)

  unless (no_total_ ropts) . mapM_ putStrLn $
    [ "Total:"
    , "--------------------"
    , padleft 20 $ showMixedAmountWithoutPrice (getSum amt)
    ]

withoutBeginDate :: ReportOpts -> ReportOpts
withoutBeginDate ropts@ReportOpts{..} = ropts{period_=p}
  where
    p = dateSpanAsPeriod $ DateSpan Nothing (periodEnd period_)

