#!/usr/bin/env stack
-- stack script --compile --resolver lts-20.8 --verbosity error --package hledger-lib --package hledger --package text

-- hledger-register-max - runs "hledger register" and prints the posting with largest running total/balance.
-- Usage:
-- hledger-register-max [REGISTERARGS]
-- hledger register-max -- [REGISTERARGS]
-- For historical balances, add -H. For negative balances, add --invert.

-- Examples:
-- $ hledger-register-max -f examples/bcexample.hledger -H checking
-- 2013-01-03 Hoogle | Payroll  Assets:US:BofA:Checking      1350.60 USD  8799.22 USD
-- $ hledger register-max -- -f examples/bcexample.hledger -H checking
-- 2013-01-03 Hoogle | Payroll  Assets:US:BofA:Checking      1350.60 USD  8799.22 USD


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import qualified "text" Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import Hledger
import Hledger.Cli
import Hledger.Cli.Main (argsToCliOpts)

-- XXX needs --help, see hledger-addon-example.hs
-- XXX shows only one posting when multiple have same balance

main = do
  args <- getArgs
  opts <- argsToCliOpts ("register" : args) []
  withJournalDo opts $ \j -> do
    let r = postingsReport (reportspec_ opts) j
    unless (null r) $ do
      let
        i = maximumBy (comparing fifth5) r
        (_, _, mdesc, p, _) = i
        d = postingDate p
        desc = fromMaybe "" mdesc
        ptxt = T.strip $ T.unlines $ first3 $ postingAsLines False True 20 15 p
      T.putStrLn $ T.unwords [showDate d, desc, "", ptxt, "", T.pack $ showMixedAmountOneLine $ fifth5 i]
