#!/usr/bin/env stack
--  stack runghc --package hledger
{-

script-example - my new script

-}

{-# LANGUAGE OverloadedStrings #-}

import Hledger.Cli.Script
import qualified Data.Text as T
import qualified Data.Text.IO as T

cmdmode = hledgerCommandMode (unlines
    ---------------------------standard terminal width-----------------------------
  ["script-example"
  ,"Usage: hledger-script-example [OPTS] [ARGS]"
  ,"or:    hledger script-example -- [OPTS] [ARGS]"
  ,"Examples:"
  ,"$ hledger-script-example         # do the thing"
  ,"$ hledger-script-example --help  # print help"
  ])
  [] [generalflagsgroup1] [] ([], Just $ argsFlag "[ARGS]")

main = do
  opts@CliOpts{reportspec_=rspec} <- getHledgerCliOpts cmdmode
  withJournal opts $ \j -> do
    putStrLn "hello"




