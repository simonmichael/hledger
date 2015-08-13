{-|
Re-export the modules of the hledger-ui program.
-}

module Hledger.UI (
                     module Hledger.UI.Main,
                     module Hledger.UI.Options,
                     tests_Hledger_UI
              )
where
import Test.HUnit

import Hledger.UI.Main
import Hledger.UI.Options

tests_Hledger_UI :: Test
tests_Hledger_UI = TestList
 [
 --  tests_Hledger_UI_Main
 --  tests_Hledger_UI_Options
 ]
