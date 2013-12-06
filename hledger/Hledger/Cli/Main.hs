{-|
hledger - a ledger-compatible accounting tool.
Copyright (c) 2007-2011 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

hledger is a partial haskell clone of John Wiegley's "ledger".  It
generates ledger-compatible register & balance reports from a plain text
journal, and demonstrates a functional implementation of ledger.
For more information, see http:\/\/hledger.org .

This module provides the main function for the hledger command-line
executable. It is exposed here so that it can be imported by eg benchmark
scripts.

You can use the command line:

> $ hledger --help

or ghci:

> $ ghci hledger
> > j <- readJournalFile Nothing Nothing "data/sample.journal"
> > register [] ["income","expenses"] j
> 2008/01/01 income               income:salary                   $-1          $-1
> 2008/06/01 gift                 income:gifts                    $-1          $-2
> 2008/06/03 eat & shop           expenses:food                    $1          $-1
>                                 expenses:supplies                $1            0
> > balance [Depth "1"] [] l
>                  $-1  assets
>                   $2  expenses
>                  $-2  income
>                   $1  liabilities
> > l <- myLedger

See "Hledger.Data.Ledger" for more examples.

-}

module Hledger.Cli.Main where

import Control.Monad
import Data.List
import Safe
import System.Console.CmdArgs.Explicit (modeHelp)
-- import System.Console.CmdArgs.Helper
import System.Environment
import System.Exit
import System.Process
import Text.Printf

import Hledger (ensureJournalFileExists)
import Hledger.Cli.Add
import Hledger.Cli.Balance
import Hledger.Cli.Balancesheet
import Hledger.Cli.Cashflow
import Hledger.Cli.Histogram
import Hledger.Cli.Incomestatement
import Hledger.Cli.Print
import Hledger.Cli.Register
import Hledger.Cli.Stats
import Hledger.Cli.Options
import Hledger.Cli.Tests
import Hledger.Cli.Utils
import Hledger.Cli.Version
import Hledger.Utils
import Hledger.Reports
import Hledger.Data.Dates

main :: IO ()
main = do

  -- Choose and run the appropriate internal or external command based
  -- on the raw command-line arguments, cmdarg's interpretation of
  -- same, and hledger-* executables in the user's PATH. A somewhat
  -- complex mishmash of cmdargs and custom processing, hence all the
  -- debugging support and tests. See also Hledger.Cli.Options and
  -- command-line.test.

  -- some preliminary (imperfect) argument parsing to supplement cmdargs
  args <- getArgs
  let
    args'                = moveFlagsAfterCommand args
    isFlag               = ("-" `isPrefixOf`)
    isNonEmptyNonFlag s  = not (isFlag s) && not (null s)
    rawcmd               = headDef "" $ takeWhile isNonEmptyNonFlag args'
    isNullCommand        = null rawcmd
    (argsbeforecmd, argsaftercmd') = break (==rawcmd) args
    argsaftercmd         = drop 1 argsaftercmd'
  when ("--debug" `elem` args) $ do
    printf "running: %s\n" prognameandversion
    printf "raw args: %s\n" (show args)
    printf "raw args rearranged for cmdargs: %s\n" (show args')
    printf "raw command might be: %s\n" (show rawcmd)
    printf "raw args before command: %s\n" (show argsbeforecmd)
    printf "raw args after command: %s\n" (show argsaftercmd)

  -- search PATH for add-ons
  addons <- getHledgerAddonCommands

  -- parse arguments with cmdargs
  opts <- argsToCliOpts args addons

  -- select an action and run it.
  let
    cmd                  = command_ opts -- the full matched internal or external command name, if any
    isInternalCommand    = not (null cmd) && not (cmd `elem` addons) -- probably
    isExternalCommand    = not (null cmd) && cmd `elem` addons -- probably
    isBadCommand         = not (null rawcmd) && null cmd
    hasHelp args         = any (`elem` args) ["--help","-h","-?"]
    hasVersion           = ("--version" `elem`)
    mainmode'            = mainmode addons
    generalHelp          = putStr $ showModeHelp mainmode'
    version              = putStrLn prognameandversion
    badCommandError      = error' ("command "++rawcmd++" is not recognized, run with no command to see a list") >> exitFailure
    f `orShowHelp` mode  = if hasHelp args then putStr (showModeHelp mode) else f
  when (debug_ opts > 0) $ do
    putStrLn $ "processed opts:\n" ++ ppShow opts
    putStrLn $ "command matched: " ++ show cmd
    putStrLn $ "isNullCommand: " ++ show isNullCommand
    putStrLn $ "isInternalCommand: " ++ show isInternalCommand
    putStrLn $ "isExternalCommand: " ++ show isExternalCommand
    putStrLn $ "isBadCommand: " ++ show isBadCommand
    d <- getCurrentDay
    putStrLn $ "date span from opts: " ++ (show $ dateSpanFromOpts d $ reportopts_ opts)
    putStrLn $ "interval from opts: " ++ (show $ intervalFromOpts $ reportopts_ opts)
    putStrLn $ "query from opts & args: " ++ (show $ queryFromOpts d $ reportopts_ opts)
  let
    dbg s = if debug_ opts then trace s else id
    runHledgerCommand
      -- high priority flags and situations. --help should be highest priority.
      | hasHelp argsbeforecmd    = dbg "--help before command, showing general help" generalHelp
      | not (hasHelp argsaftercmd) && (hasVersion argsbeforecmd || (hasVersion argsaftercmd && isInternalCommand))
                                 = version
      -- \| (null externalcmd) && "binary-filename" `inRawOpts` rawopts = putStrLn $ binaryfilename progname
      -- \| "--browse-args" `elem` args     = System.Console.CmdArgs.Helper.execute "cmdargs-browser" mainmode' args >>= (putStr . show)
      | isNullCommand            = dbg "no command, showing general help" generalHelp
      | isBadCommand             = badCommandError

      -- internal commands
      | cmd == "activity"        = withJournalDo opts histogram       `orShowHelp` activitymode
      | cmd == "add"             = (journalFilePathFromOpts opts >>= ensureJournalFileExists >> withJournalDo opts add) `orShowHelp` addmode
      | cmd == "balance"         = withJournalDo opts balance         `orShowHelp` balancemode
      | cmd == "balancesheet"    = withJournalDo opts balancesheet    `orShowHelp` balancesheetmode
      | cmd == "cashflow"        = withJournalDo opts cashflow        `orShowHelp` cashflowmode
      | cmd == "incomestatement" = withJournalDo opts incomestatement `orShowHelp` incomestatementmode
      | cmd == "print"           = withJournalDo opts print'          `orShowHelp` printmode
      | cmd == "register"        = withJournalDo opts register        `orShowHelp` registermode
      | cmd == "stats"           = withJournalDo opts stats           `orShowHelp` statsmode
      | cmd == "test"            = test' opts                         `orShowHelp` testmode

      -- an external command
      | isExternalCommand = do
          let shellcmd = printf "%s-%s %s" progname cmd (unwords' argsaftercmd)
          when (debug_ opts) $ do
            printf "external command selected: %s\n" cmd
            printf "external command arguments: %s\n" (show argsaftercmd)
            printf "running shell command: %s\n" (show shellcmd)
          system shellcmd >>= exitWith

      -- deprecated commands
      | cmd == "convert"         = error' (modeHelp convertmode) >> exitFailure

      -- shouldn't reach here
      | otherwise                = optserror ("could not understand the arguments "++show args) >> exitFailure

  runHledgerCommand


-- tests_runHledgerCommand = [
--   -- "runHledgerCommand" ~: do
--   --   let opts = defreportopts{query_="expenses"}
--   --   d <- getCurrentDay
--   --   runHledgerCommand addons opts@CliOpts{command_=cmd} args

--  ]


