{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Hledger.Cli.BalanceView (
  BalanceView(..)
 ,balanceviewmode
 ,balanceviewReport
) where

import Data.Time.Calendar (Day)
import Data.List (intercalate)
import Data.Monoid (Sum(..), (<>))
import System.Console.CmdArgs.Explicit

import Hledger
import Hledger.Cli.Balance
import Hledger.Cli.CliOptions

data BalanceView = BV { bvmode    :: String
                      , bvaliases :: [String]
                      , bvhelp    :: String
                      , bvname    :: String
                      , bvqueries :: [(String, Journal -> Query)]
                      }

balanceviewmode :: BalanceView -> Mode RawOpts
balanceviewmode bv@BV{..} = (defCommandMode $ bvmode : bvaliases) {
  modeHelp = bvhelp `withAliases` bvaliases
 ,modeGroupFlags = Group {
     groupUnnamed = [
      flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show accounts as a list"
     ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
     ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }

balanceviewQueryReport
    :: ReportOpts
    -> Day
    -> Journal
    -> String
    -> (Journal -> Query)
    -> ([String], Sum MixedAmount)
balanceviewQueryReport ropts d j t q = ([view], Sum amt)
    where
      q' = And [queryFromOpts d (withoutBeginDate ropts), q j]
      rep@(_ , amt) = balanceReport ropts q' j
      view = intercalate "\n" [t <> ":", balanceReportAsText ropts rep]

balanceviewReport :: BalanceView -> CliOpts -> Journal -> IO ()
balanceviewReport BV{..} CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  let (views, amt) = foldMap (uncurry (balanceviewQueryReport ropts d j)) bvqueries
  mapM_ putStrLn (bvname : "" : views)
  putStrLn . unlines $
    [ "Total:"
    , "--------------------"
    , padleft 20 $ showMixedAmountWithoutPrice (getSum amt)
    ]

withoutBeginDate :: ReportOpts -> ReportOpts
withoutBeginDate ropts@ReportOpts{..} = ropts{period_=p}
  where
    p = dateSpanAsPeriod $ DateSpan Nothing (periodEnd period_)

