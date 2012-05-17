module Hledger (
                module Hledger.Data
               ,module Hledger.Read
               ,module Hledger.Reports
               ,module Hledger.Utils
               ,tests_Hledger
)
where
import Test.HUnit

import Hledger.Data
import Hledger.Read
import Hledger.Reports
import Hledger.Utils

tests_Hledger = TestList
    [
     tests_Hledger_Data
    ,tests_Hledger_Data_Query
    ,tests_Hledger_Read
    ,tests_Hledger_Reports
    ]
