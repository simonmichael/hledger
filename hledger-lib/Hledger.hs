{-# LANGUAGE OverloadedStrings #-}

module Hledger (
  module X
 ,tests_Hledger
 ,Hledger.easytests
)
where

import           Hledger.Data    as X hiding (easytests)
import qualified Hledger.Data    (easytests)
import           Hledger.Read    as X hiding (samplejournal, easytests)
import qualified Hledger.Read    (easytests)
import           Hledger.Reports as X
import           Hledger.Query   as X
import           Hledger.Utils   as X

tests_Hledger = TestList
    [
     tests_Hledger_Data
    ,tests_Hledger_Query
    ,tests_Hledger_Read
    ,tests_Hledger_Reports
    ,tests_Hledger_Utils
    ]

easytests = tests "Hledger" [
   Hledger.Data.easytests
  ,Hledger.Read.easytests
  ]
