{-# LANGUAGE CPP #-}
{-| 

The Commands package defines all the commands offered by the hledger
application, like \"register\" and \"balance\".  This module exports all
the commands; you can also import individual modules if you prefer.

-}

module Commands.All (
                     module Commands.Add,
                     module Commands.Balance,
                     module Commands.Convert,
                     module Commands.Histogram,
                     module Commands.Print,
                     module Commands.Register,
                     module Commands.Stats,
#ifdef VTY
                     module Commands.UI,
#endif
#if defined(WEB) || defined(WEBHAPPSTACK)
                     module Commands.Web,
#endif
#ifdef CHART
                     module Commands.Chart,
#endif
                     tests_Commands
              )
where
import Commands.Add
import Commands.Balance
import Commands.Convert
import Commands.Histogram
import Commands.Print
import Commands.Register
import Commands.Stats
#ifdef VTY
import Commands.UI
#endif
#if defined(WEB) || defined(WEBHAPPSTACK)
import Commands.Web
#endif
#ifdef CHART
import Commands.Chart
#endif
import Test.HUnit (Test(TestList))


tests_Commands = TestList
    [
--      Commands.Add.tests_Add
--     ,Commands.Balance.tests_Balance
--     ,Commands.Convert.tests_Convert
--     ,Commands.Histogram.tests_Histogram
--     ,Commands.Print.tests_Print
     Commands.Register.tests_Register
--     ,Commands.Stats.tests_Stats
-- #ifdef VTY
--     ,Commands.UI.tests_UI
-- #endif
-- #if defined(WEB) || defined(WEBHAPPSTACK)
--     ,Commands.Web.tests_Web
-- #endif
-- #ifdef CHART
--     ,Commands.Chart.tests_Chart
-- #endif
    ]
