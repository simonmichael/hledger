{-|
Re-export the modules of the hledger-web program.
-}

module Hledger.Web (
                     module Hledger.Web.Foundation,
                     module Hledger.Web.Application,
                     module Hledger.Web.Handlers,
                     module Hledger.Web.Import,
                     module Hledger.Web.Options,
                     module Hledger.Web.Settings,
                     module Hledger.Web.Settings.StaticFiles,
                     tests_Hledger_Web
              )
where
import Test.HUnit

import Hledger.Web.Foundation
import Hledger.Web.Application
import Hledger.Web.Handlers
import Hledger.Web.Import
import Hledger.Web.Options
import Hledger.Web.Settings
import Hledger.Web.Settings.StaticFiles

tests_Hledger_Web :: Test
tests_Hledger_Web = TestList
 [
 --  tests_Hledger_Web_Foundation
 -- ,tests_Hledger_Web_Application
 -- ,tests_Hledger_Web_EmbeddedFiles
 -- ,tests_Hledger_Web_Handlers
 -- ,tests_Hledger_Web_Settings
 -- ,tests_Hledger_Web_Settings_StaticFiles
 ]
