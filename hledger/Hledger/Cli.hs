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
                     versionString
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

-- | The program name and version string for this build of the hledger tool,
-- including any git info available at build time.
prognameandversion :: String
prognameandversion = versionString progname packageversion

-- | A helper to generate the best version string we can from the given 
-- program name and package version strings, current os and architecture,
-- and any git info available at build time (commit hash, commit date, branch
-- name, patchlevel since latest release tag for that program's package).
-- Typically called for programs "hledger", "hledger-ui", or "hledger-web".
--
-- The git info changes whenever any file in the repository changes. 
-- Keeping this template haskell call here and not down in Hledger.Cli.Version
-- helps reduce the number of modules recompiled.
versionString :: ProgramName -> PackageVersion -> String
versionString = versionStringWith $$tGitInfoCwdTry

-- unit tests (tests_Hledger_Cli) are defined in Hledger.Cli.Commands
