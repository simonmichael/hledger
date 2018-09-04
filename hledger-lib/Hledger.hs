{-# LANGUAGE OverloadedStrings #-}

module Hledger (
  module X
 ,tests_Hledger
 ,easytests_Hledger
)
where

import           Hledger.Data    as X
import           Hledger.Read    as X
import           Hledger.Reports as X
import           Hledger.Query   as X
import           Hledger.Utils   as X

tests_Hledger = TestList
    [
     tests_Hledger_Data
    ,tests_Hledger_Reports
    ]

easytests_Hledger = tests "Hledger" [
   easytests_Data
  ,easytests_Read
  ,easytests_Query
  ,easytests_Utils
  ]
