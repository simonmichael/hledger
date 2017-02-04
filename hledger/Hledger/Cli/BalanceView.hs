{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Hledger.Cli.BalanceView (
  BalanceView(..)
 ,balanceviewReport
) where

import Data.Time.Calendar
import Data.List
import Data.Monoid (Sum(..), (<>))
import qualified Data.Text as T

import Hledger
import Hledger.Cli.Balance
import Hledger.Cli.CliOptions

data BalanceView = BV { bvname    :: String
                      , bvqueries :: [(String, Journal -> Query)]
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

