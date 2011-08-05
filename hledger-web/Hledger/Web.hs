{-|
Re-export the modules of the hledger-web program.
-}

module Hledger.Web (
                     module Hledger.Web.App,
                     module Hledger.Web.AppRun,
                     module Hledger.Web.EmbeddedFiles,
                     module Hledger.Web.Handlers,
                     module Hledger.Web.Settings,
                     module Hledger.Web.StaticFiles,
                     tests_Hledger_Web
              )
where
import Test.HUnit

import Hledger.Web.App
import Hledger.Web.AppRun
import Hledger.Web.EmbeddedFiles
import Hledger.Web.Handlers
import Hledger.Web.Settings
import Hledger.Web.StaticFiles

tests_Hledger_Web :: Test
tests_Hledger_Web = TestList
 [
 --  tests_Hledger_Web_App
 -- ,tests_Hledger_Web_AppRun
 -- ,tests_Hledger_Web_EmbeddedFiles
 -- ,tests_Hledger_Web_Handlers
 -- ,tests_Hledger_Web_Settings
 -- ,tests_Hledger_Web_StaticFiles
 ]
