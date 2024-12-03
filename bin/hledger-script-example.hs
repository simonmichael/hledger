#!/usr/bin/env stack
-- stack runghc --verbosity error --package hledger

-- This script is run by the "stack runghc" command above.
-- This command expects to be running in a copy of the hledger source code.
-- Its advantage is that it uses that latest hledger source version.
-- To show more progress output, change --verbosity to info.
-- The main haskell package needed for scripting is "hledger".
-- To make more packages available for import, add more --package options.
--
-- For more robustness, use the "stack script" command below instead.
-- This uses a released version of hledger, and does not need the hledger source code.
-- The version of hledger (and haskell libraries, and ghc) is determined by
-- the stackage snapshot (lts-X.Y or nightly-YYYY-MM-DD).
--
-- stack script --snapshot lts-22.27 --verbosity error --package hledger --package text
--  (Uses the specified snapshot with its hledger and ghc, https://www.stackage.org/lts-22.27)
--
-- stack script --snapshot nightly-2024-09-26 --verbosity error --package hledger --package hledger-lib --package text --compile
--  (Uses a newer snapshot, hledger and ghc. The --compile flag compiles the script to a faster binary.)
--
------------------------------------78----------------------------------------

{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Hledger.Cli.Script
import qualified "text" Data.Text as T
import qualified "text" Data.Text.IO as T

cmdmode = hledgerCommandMode (unlines
    -- Command name, then --help text, then _FLAGS; empty help lines get stripped:
  ["script-example"
  ,"This is an example of a (hledger-lib-using) hledger script."
  ,"Usage: hledger-script-example [OPTS] [ARGS]"
  ,"or:    hledger script-example -- [OPTS] [ARGS]"
  ,"Save it under another name and customise it."
  ,"The hledger- name makes it appear in hledger's commands list."
  ,"Examples:"
  ,"$ hledger-script-example --help"
  ,"(this help)"
    ------------------------------------78----------------------------------------
  ,""
  ,"_FLAGS"
  ])
  [] [generalflagsgroup1] [] ([], Just $ argsFlag "[ARGS]")  -- or Nothing

main = do
  opts@CliOpts{reportspec_=rspec} <- getHledgerCliOpts cmdmode
  withJournalDo opts $ \j -> do
    putStrLn "it worked! print something more useful here"


-- Examples:
-- See also bin/*.hs


-- Count transactions, possibly filtered by a query:

--    d <- getCurrentDay
--    let
--      q = _rsQuery rspec
--      ts = filter (q `matchesTransaction`) $ jtxns $ journalApplyValuationFromOpts rspec j
--    printf "File %s: %d transactions\n" (journalFilePath j) (length ts)


-- register-max:

--   withJournalDo opts $ \j -> do
--     let
--       postingReportItems = postingsReport rspec j
--       maxbal = fifth5 $ maximumBy (comparing fifth5) r
--       is = filter ((== maxbal).fifth5) r
--     mapM_ printItem is
--
-- printItem (_, _, _, p, bal) = do
--   let
--     d      = postingDate p
--     mt     = ptransaction p
--     desc   = fmt  30 $ maybe "-" tdescription mt
--     acct   = fmt  30 $ paccount p
--     amt    = fmta 12 $ T.pack $ showMixedAmountOneLine $ pamount p
--     baltxt = fmta 12 $ T.pack $ showMixedAmountOneLine bal
--   T.putStrLn $ T.unwords [showDate d, desc, "", acct, "", amt, " ", baltxt]
--   where
--     fmt w  = formatText True (Just w) (Just w) . textElideRight w
--     fmta w = formatText False (Just w) Nothing


-- Using [s|...|] for multiline string literals (requires string-qq package and {-# LANGUAGE QuasiQuotes #-}):

-- cmdmode = hledgerCommandMode (unlines
--     -- Command name, then --help text, then _FLAGS; empty help lines get stripped:
--   [s| script-example
-- This is an example of a (hledger-lib-using) hledger script."
-- Usage: hledger-script-example [OPTS] [ARGS]"
-- or:    hledger script-example -- [OPTS] [ARGS]"
-- Save it under another name and customise it."
-- The hledger- name makes it appear in hledger's commands list."
-- Examples:"
-- $ hledger-script-example --help"
-- (this help)"
--
-- _FLAGS
--   |]
--     ------------------------------------78----------------------------------------
--   [] [generalflagsgroup1] [] ([], Just $ argsFlag "[ARGS]")  -- or Nothing


{- 
More help:

This an example of an addon command (an executable named hledger-*).
It supports many of the usual hledger options; run it with -h/--help
to see them. When you want to create a new hledger command, 
save this script under a new name, somewhere in $PATH, give it
execute permission, and start tweaking the code.

Requirements:

This is a stack script, best run or compiled with stack.
Once compiled it doesn't require stack.
If you prefer you can adapt it to be a cabal script,
or you can manually install the required haskell libraries
(see above) and then run/compile it just with ghc or runghc.

If run/compiled from inside the hledger source tree, it will use that hledger
version and the libs of the stackage resolver in stack.yaml.
If run/compiled from outside the hledger source tree, it will use the hledger
and libs of the resolver in ~/.stack/global-project/stack.yaml.
Or you can specify a --resolver in the stack command above.

Usage:

Executing this script will cause stack to run it in interpreted mode:

$ hledger-script-example.hs

Or you can compile first:

$ stack ghc hledger-script-example.hs --package hledger --package string-qq
$ hledger-script-example

Whether compiled or not, you can also run it as a hledger subcommand, if it is in $PATH:

$ hledger script-example

-}
