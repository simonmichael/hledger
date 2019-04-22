{-|

Hledger.Cli re-exports the options, utilities and commands provided by
the hledger command-line program. This module also aggregates the
built-in unit tests defined throughout hledger and hledger-lib, and
adds some more which are easier to define here.

-}

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Cli (
                     module Hledger.Cli.CliOptions,
                     module Hledger.Cli.Commands,
                     module Hledger.Cli.DocFiles,
                     module Hledger.Cli.Utils,
                     module Hledger.Cli.Version,
                     module Hledger,
                     tests_Cli,
                     module System.Console.CmdArgs.Explicit
              )
where
import System.Console.CmdArgs.Explicit hiding (Name) -- don't clash with hledger-ui

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands
import Hledger.Cli.DocFiles
import Hledger.Cli.Utils
import Hledger.Cli.Version

-- unit tests for code under Hledger.Cli (hledger-lib tests not included)
tests_Cli = tests "Hledger.Cli" [
   tests_Cli_Utils
  ,tests_Commands
  ]
