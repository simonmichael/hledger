{-|

The @balancesheet@ command prints a fairly standard balance sheet.

-}

module Hledger.Cli.Balancesheet (
  balancesheet
 ,tests_Hledger_Cli_Balancesheet
) where

import Data.List
import Test.HUnit

import Hledger
import Prelude hiding (putStr)
import Hledger.Utils.UTF8IOCompat (putStr)
import Hledger.Cli.Options
import Hledger.Cli.Balance


-- | Print a standard balancesheet.
balancesheet :: CliOpts -> Journal -> IO ()
balancesheet CliOpts{reportopts_=ropts} j = do
  let lines = case formatFromOpts ropts of
            Left err -> [err]
            Right _ -> accountsReportAsText ropts $ accountsReport2 ropts (journalBalanceSheetAccountQuery j) j
  putStr $ unlines lines

tests_Hledger_Cli_Balancesheet = TestList
 [
 ]
