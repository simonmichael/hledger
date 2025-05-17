-- hledger-web package test suite. Also runnable via hledger-web --test.

module Main where

import Hledger.Web.Test (hledgerWebTest)

main :: IO ()
main = hledgerWebTest
