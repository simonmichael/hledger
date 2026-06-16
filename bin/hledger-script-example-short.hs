#!/usr/bin/env stack
-- stack runghc --package hledger
-- or, for more setup/build progress output:
-- stack runghc --package hledger --verbosity info

-- hledger-script-example-short - a short hledger stack script example

{-# LANGUAGE OverloadedStrings #-}

import Hledger.Cli.Script
import Data.Text qualified as T
import Data.Text.IO qualified as T

cmdmode = hledgerCommandMode (unlines
    ---------------------------standard terminal width-----------------------------
  ["script-example-short"
  ,"Usage: hledger-script-example-short [OPTS] [ARGS]"
  ,"or:    hledger script-example-short -- [OPTS] [ARGS]"
  ,"Examples:"
  ,"$ hledger-script-example-short         # do the thing"
  ,"$ hledger-script-example-short --help  # print help"
  ])
  [] [generalflagsgroup1] [] ([], Just $ argsFlag "[ARGS]")

main = do
  opts@CliOpts{reportspec_=rspec} <- getHledgerCliOpts cmdmode
  withJournal opts $ \j -> do
    putStrLn $ "successfully read journal: " <> journalFilePath j
