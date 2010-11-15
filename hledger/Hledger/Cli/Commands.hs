{-# LANGUAGE CPP #-}
{-| 

The Commands package defines all the commands offered by the hledger
application, like \"register\" and \"balance\".  This module exports all
the commands; you can also import individual modules if you prefer.

-}

module Hledger.Cli.Commands (
                     module Hledger.Cli.Add,
                     module Hledger.Cli.Balance,
                     module Hledger.Cli.Convert,
                     module Hledger.Cli.Histogram,
                     module Hledger.Cli.Print,
                     module Hledger.Cli.Register,
                     module Hledger.Cli.Stats,
                     tests_Hledger_Commands
              )
where
import Hledger.Cli.Add
import Hledger.Cli.Balance
import Hledger.Cli.Convert
import Hledger.Cli.Histogram
import Hledger.Cli.Print
import Hledger.Cli.Register
import Hledger.Cli.Stats
import Test.HUnit (Test(TestList))


tests_Hledger_Commands = TestList
    [
--      Hledger.Cli.Add.tests_Add
--     ,Hledger.Cli.Balance.tests_Balance
     Hledger.Cli.Convert.tests_Convert
--     ,Hledger.Cli.Histogram.tests_Histogram
--     ,Hledger.Cli.Print.tests_Print
    ,Hledger.Cli.Register.tests_Register
--     ,Hledger.Cli.Stats.tests_Stats
    ]
