{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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
     ,flagNone ["value","V"] (setboolopt "value") "convert amounts to their market value on the report end date (using the most recent applicable market price, if any)"
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
    -> Day
    -> Maybe Day
    -> Journal
    -> String
    -> (Journal -> Query)
    -> ([String], Sum MixedAmount)
balanceviewQueryReport ropts currDay reportEnd j t q = ([view], Sum amt)
    where
      q' = And [queryFromOpts currDay (withoutBeginDate ropts), q j]
      convert | value_ ropts = maybe id (balanceReportValue j) reportEnd
              | otherwise    = id
      rep@(_ , amt) = convert $ balanceReport ropts q' j
      view = intercalate "\n" [t <> ":", balanceReportAsText ropts rep]

balanceviewReport :: BalanceView -> CliOpts -> Journal -> IO ()
balanceviewReport BV{..} CliOpts{reportopts_=ropts} j = do
  currDay   <- getCurrentDay
  reportEnd <- reportEndDate j ropts
  let (views, amt) =
        foldMap (uncurry (balanceviewQueryReport ropts currDay reportEnd j))
           bvqueries
  mapM_ putStrLn (bvname : "" : views)

  unless (no_total_ ropts) .  putStrLn . unlines $
    [ "Total:"
    , "--------------------"
    , padleft 20 $ showMixedAmountWithoutPrice (getSum amt)
    ]

withoutBeginDate :: ReportOpts -> ReportOpts
withoutBeginDate ropts@ReportOpts{..} = ropts{period_=p}
  where
    p = dateSpanAsPeriod $ DateSpan Nothing (periodEnd period_)

