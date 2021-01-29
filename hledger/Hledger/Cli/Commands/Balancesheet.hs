{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-|

The @balancesheet@ command prints a simple balance sheet.

-}

module Hledger.Cli.Commands.Balancesheet (
  balancesheetmode
 ,balancesheet
) where

import System.Console.CmdArgs.Explicit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.CompoundBalanceCommand

balancesheetSpec = CompoundBalanceCommandSpec {
  cbcdoc      = $(embedFileRelative "Hledger/Cli/Commands/Balancesheet.txt"),
  cbctitle    = "Balance Sheet",
  cbcqueries  = [
     CBCSubreportSpec{
      cbcsubreporttitle="Assets"
     ,cbcsubreportquery=journalAssetAccountQuery
     ,cbcsubreportoptions=(\ropts -> ropts{normalbalance_=Just NormallyPositive})
     ,cbcsubreporttransform=id
     ,cbcsubreportincreasestotal=True
     }
    ,CBCSubreportSpec{
      cbcsubreporttitle="Liabilities"
     ,cbcsubreportquery=journalLiabilityAccountQuery
     ,cbcsubreportoptions=(\ropts -> ropts{normalbalance_=Just NormallyNegative})
     ,cbcsubreporttransform=fmap maNegate
     ,cbcsubreportincreasestotal=False
     }
    ],
  cbctype     = HistoricalBalance
}

balancesheetmode :: Mode RawOpts
balancesheetmode = compoundBalanceCommandMode balancesheetSpec

balancesheet :: CliOpts -> Journal -> IO ()
balancesheet = compoundBalanceCommand balancesheetSpec
