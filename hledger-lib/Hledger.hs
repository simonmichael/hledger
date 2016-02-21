module Hledger (
  module X
 ,tests_Hledger
)
where
import           Test.HUnit

import           Hledger.Data    as X
import           Hledger.Query   as X
import           Hledger.Read    as X hiding (samplejournal)
import           Hledger.Reports as X
import           Hledger.Utils   as X

tests_Hledger = TestList
    [
     tests_Hledger_Data
    ,tests_Hledger_Query
    ,tests_Hledger_Read
    ,tests_Hledger_Reports
    ]
