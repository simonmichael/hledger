{-|

The @incomestatement@ command prints a fairly standard income statement (profit & loss) report.

-}

module Hledger.Cli.Incomestatement (
  incomestatement
 ,tests_Hledger_Cli_Incomestatement
) where

import Data.List
import Test.HUnit

import Hledger
import Prelude hiding (putStr)
import Hledger.Utils.UTF8IOCompat (putStr)
import Hledger.Cli.Options
import Hledger.Cli.Balance


-- | Print a standard income statement.
incomestatement :: CliOpts -> Journal -> IO ()
incomestatement CliOpts{reportopts_=ropts} j = do
  let lines = case formatFromOpts ropts of
            Left err -> [err]
            Right _ -> accountsReportAsText ropts $ accountsReport2 ropts (journalProfitAndLossAccountMatcher j) j
  putStr $ unlines lines

tests_Hledger_Cli_Incomestatement = TestList
 [
 ]
