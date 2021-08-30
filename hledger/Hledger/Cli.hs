{-# LANGUAGE TemplateHaskell #-}
{-|

Hledger.Cli re-exports the options, utilities and commands provided by
the hledger command-line program. This module also aggregates the
built-in unit tests defined throughout hledger and hledger-lib, and
adds some more which are easier to define here.

-}

module Hledger.Cli (
                     module Hledger.Cli.CliOptions,
                     module Hledger.Cli.Commands,
                     module Hledger.Cli.DocFiles,
                     module Hledger.Cli.Utils,
                     module Hledger.Cli.Version,
                     module Hledger,
                     module System.Console.CmdArgs.Explicit,
                     prognameandversion,
                     versionStringForProgname
              )
where

import GitHash (tGitInfoCwdTry)
import System.Console.CmdArgs.Explicit hiding (Name) -- don't clash with hledger-ui

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands
import Hledger.Cli.DocFiles
import Hledger.Cli.Utils
import Hledger.Cli.Version

-- | The program name and the best version information we can obtain
-- from git describe or build variables.
prognameandversion = versionStringForProgname progname
versionStringForProgname = versionStringFor $$tGitInfoCwdTry

-- unit tests (tests_Hledger_Cli) are defined in Hledger.Cli.Commands
