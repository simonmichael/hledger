{-# LANGUAGE CPP #-}
{-| 

The Commands package defines all the commands offered by the hledger
application, like \"register\" and \"balance\".  This module exports all
the commands; you can also import individual modules if you prefer.

-}

module Hledger.Cli.Commands.All (
                     module Hledger.Cli.Commands.Add,
                     module Hledger.Cli.Commands.Balance,
                     module Hledger.Cli.Commands.Convert,
                     module Hledger.Cli.Commands.Histogram,
                     module Hledger.Cli.Commands.Print,
                     module Hledger.Cli.Commands.Register,
                     module Hledger.Cli.Commands.Stats,
#ifdef VTY
                     module Hledger.Cli.Commands.Vty,
#endif
#if defined(WEB)
                     module Hledger.Cli.Commands.Web,
#elif defined(WEBYESOD)
                     module Hledger.Cli.Commands.WebYesod,
#endif
#ifdef CHART
                     module Hledger.Cli.Commands.Chart,
#endif
                     tests_Hledger_Commands
              )
where
import Hledger.Cli.Commands.Add
import Hledger.Cli.Commands.Balance
import Hledger.Cli.Commands.Convert
import Hledger.Cli.Commands.Histogram
import Hledger.Cli.Commands.Print
import Hledger.Cli.Commands.Register
import Hledger.Cli.Commands.Stats
#ifdef VTY
import Hledger.Cli.Commands.Vty
#endif
#if defined(WEB)
import Hledger.Cli.Commands.Web
#elif defined(WEBYESOD)
import Hledger.Cli.Commands.WebYesod
#endif
#ifdef CHART
import Hledger.Cli.Commands.Chart
#endif
import Test.HUnit (Test(TestList))


tests_Hledger_Commands = TestList
    [
--      Hledger.Cli.Commands.Add.tests_Add
--     ,Hledger.Cli.Commands.Balance.tests_Balance
     Hledger.Cli.Commands.Convert.tests_Convert
--     ,Hledger.Cli.Commands.Histogram.tests_Histogram
--     ,Hledger.Cli.Commands.Print.tests_Print
    ,Hledger.Cli.Commands.Register.tests_Register
--     ,Hledger.Cli.Commands.Stats.tests_Stats
    ]
-- #ifdef VTY
--     ,Hledger.Cli.Commands.Vty.tests_Vty
-- #endif
-- #if defined(WEB)
--     ,Hledger.Cli.Commands.Web.tests_Web
-- #elif defined(WEBYESOD)
--     ,Hledger.Cli.Commands.WebYesod.tests_Web
-- #endif
-- #ifdef CHART
--     ,Hledger.Cli.Commands.Chart.tests_Chart
-- #endif
