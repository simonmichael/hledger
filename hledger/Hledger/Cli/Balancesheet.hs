{-# LANGUAGE QuasiQuotes, RecordWildCards, NoCPP #-}
{-|

The @balancesheet@ command prints a simple balance sheet.

-}

module Hledger.Cli.Balancesheet (
  balancesheetmode
 ,balancesheet
 ,tests_Hledger_Cli_Balancesheet
) where

import qualified Data.Text.Lazy.IO as LT
import System.Console.CmdArgs.Explicit
import Test.HUnit
import Text.Shakespeare.Text

import Hledger
import Hledger.Cli.CliOptions
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

-- | Print a simple balance sheet.
balancesheet :: CliOpts -> Journal -> IO ()
balancesheet CliOpts{reportopts_=ropts} j = do
  -- let lines = case lineFormatFromOpts ropts of Left err, Right ...
  d <- getCurrentDay
  let q = queryFromOpts d (withoutBeginDate ropts)
      assetreport@(_,assets)          = balanceReport ropts (And [q, journalAssetAccountQuery j]) j
      liabilityreport@(_,liabilities) = balanceReport ropts (And [q, journalLiabilityAccountQuery j]) j
      total = assets + liabilities
  LT.putStr $ [lt|Balance Sheet

Assets:
#{balanceReportAsText ropts assetreport}
Liabilities:
#{balanceReportAsText ropts liabilityreport}
Total:
--------------------
#{padleft 20 $ showMixedAmountWithoutPrice total}
|]

withoutBeginDate :: ReportOpts -> ReportOpts
withoutBeginDate ropts@ReportOpts{..} = ropts{begin_=Nothing, period_=p}
  where p = case period_ of Nothing -> Nothing
                            Just (i, DateSpan _ e) -> Just (i, DateSpan Nothing e)

tests_Hledger_Cli_Balancesheet :: Test
tests_Hledger_Cli_Balancesheet = TestList
 [
 ]
