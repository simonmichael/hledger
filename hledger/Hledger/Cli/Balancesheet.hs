{-# LANGUAGE QuasiQuotes, RecordWildCards, NoCPP #-}
{-|

The @balancesheet@ command prints a simple balance sheet.

-}

module Hledger.Cli.Balancesheet (
  balancesheetmode
 ,balancesheet
 ,balancesheetPrint
 ,tests_Hledger_Cli_Balancesheet
) where

import qualified Data.Text.Lazy.IO (putStr)
import Data.Time.Calendar (Day)
import System.Console.CmdArgs.Explicit
import Test.HUnit
import Text.Shakespeare.Text

import Hledger
import Hledger.Cli.Options
import Hledger.Cli.Balance


balancesheetmode :: Mode RawOpts
balancesheetmode = (defCommandMode $ ["balancesheet"]++aliases) {
  modeHelp = "show a balance sheet" `withAliases` aliases
 ,modeGroupFlags = Group {
     groupUnnamed = [
      flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show accounts as a list"
     ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
     ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
  where aliases = ["bs"]

-- | Calculate a simple balance sheet as of a particular day, returning a summary of the assets, liabilities, and net worth (assets - liabilities)
balancesheet :: Day -> CliOpts -> Journal -> (String, String, String)
balancesheet d CliOpts{reportopts_=ropts} j = -- do
  let q = queryFromOpts d (withoutBeginDate ropts{end_=Just d})
      assetreport@(_,assets)          = balanceReport ropts{end_=Just d} (And [q, journalAssetAccountQuery j]) j
      liabilityreport@(_,liabilities) = balanceReport ropts{end_=Just d} (And [q, journalLiabilityAccountQuery j]) j
      total = assets + liabilities
     in (  unlines $ balanceReportAsText ropts{no_total_=True} assetreport
         , unlines $ balanceReportAsText ropts{no_total_=True} liabilityreport
                                                            -- ([((AccountName, AccountName, Int), MixedAmount)], MixedAmount)
         , unlines $ balanceReportAsText ropts{no_total_=True} ([(("",          "",          1),   total)]      , total)
         )

-- | Print a formatted version of a balancesheet
balancesheetPrint :: Day -> CliOpts -> Journal -> IO ()
balancesheetPrint d c j = do let (a,l,n) = balancesheet d c j
                             Data.Text.Lazy.IO.putStr $ [lt|Balance Sheet
Assets:
#{a}
Liabilities:
#{l}
Total:
--------------------
#{n}
|]

withoutBeginDate :: ReportOpts -> ReportOpts
withoutBeginDate ropts@ReportOpts{..} = ropts{begin_=Nothing, period_=p}
  where p = case period_ of Nothing -> Nothing
                            Just (i, DateSpan _ e) -> Just (i, DateSpan Nothing e)

tests_Hledger_Cli_Balancesheet :: Test
tests_Hledger_Cli_Balancesheet = TestList
 [
 ]
