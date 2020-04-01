{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-|

The @balancesheet@ command prints a simple balance sheet.

-}

module Hledger.Cli.Commands.Balancesheet (
  balancesheetmode
 ,balancesheet
) where

import System.Console.CmdArgs.Explicit
import Data.Map as M
import Data.Text as T

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.CompoundBalanceCommand
import Hledger.Data.Types

balancesheetSpec j = CompoundBalanceCommandSpec {
  cbcdoc      = $(embedFileRelative "Hledger/Cli/Commands/Balancesheet.txt"),
  cbctitle    = "Balance Sheet",
  cbcqueries  = [
     CBCSubreportSpec{
      cbcsubreporttitle=T.unpack $ maybe "Assets" (Prelude.head . accountNameComponents . Prelude.head) (M.lookup Asset (jdeclaredaccounttypes j))
     ,cbcsubreportquery=journalAssetAccountQuery
     ,cbcsubreportnormalsign=NormallyPositive
     ,cbcsubreportincreasestotal=True
     }
    ,CBCSubreportSpec{
      cbcsubreporttitle=T.unpack $ maybe "Liabilities" (Prelude.head . accountNameComponents . Prelude.head) (M.lookup Liability (jdeclaredaccounttypes j))
     ,cbcsubreportquery=journalLiabilityAccountQuery
     ,cbcsubreportnormalsign=NormallyNegative
     ,cbcsubreportincreasestotal=False
     }
    ],
  cbctype     = HistoricalBalance
}

balancesheetmode :: Mode RawOpts
balancesheetmode = compoundBalanceCommandMode $ balancesheetSpec nulljournal

balancesheet :: CliOpts -> Journal -> IO ()
balancesheet opts j = compoundBalanceCommand (balancesheetSpec j) opts j

