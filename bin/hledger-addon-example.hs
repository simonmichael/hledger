#!/usr/bin/env stack
-- stack runghc --verbosity info --package hledger --package string-qq
--resolver  lts-20.1

{-
hledger-addon-example - a hledger addon command template.

This an example of an addon command (an executable named hledger-*).
By default it reads your default journal and prints the number of
transactions. It supports many of the usual hledger options; run it
with -h/--help to see them. When you want to create a new hledger
command, save this script under a new name, somewhere in $PATH,
keeping it executable, and start tweaking the code.

Requirements:

This is a stack script, requiring stack to run. hledger addons do not
have to be stack scripts, but this one is, as they work well for this.
If you prefer you can adapt it to be a cabal script, or you can
install the required haskell libraries (see above) and then
run/compile it with a suitable runghc/ghc command.

The script may require specific versions of the libraries.
If run/compiled from inside the hledger source tree, it will use that hledger
version and the libs of the stackage resolver in stack.yaml.
If run/compiled from outside the hledger source tree, it will use the hledger
and libs of the resolver in ~/.stack/global-project/stack.yaml.
Or you can uncomment --resolver above to use another resolver.

Usage:

Executing this script will cause stack to run it in interpreted mode:

$ hledger-addon-example.hs

Or you can compile first:

$ stack ghc hledger-addon-example.hs --package hledger --package string-qq
$ hledger-addon-example

Whether compiled or not, you can also run it as a hledger subcommand, if it is in $PATH:

$ hledger addon-example

-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.String.QQ (s)
import Text.Printf
import Hledger
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  -- Command name and help text goes here. Current limitations:
  -- help text must be above _FLAGS, blank lines will not be displayed.
  [s| addon-example
Print the number of transactions in the journal.

_FLAGS
  |]
  [] 
  [generalflagsgroup1]
  []
  ([], Nothing) -- Just $ argsFlag "[QUERY]")
------------------------------------------------------------------------------

main :: IO ()
main = do
  opts@CliOpts{reportspec_=rspec} <- getHledgerCliOpts cmdmode
  withJournalDo opts $ \j -> do
    d <- getCurrentDay
    let
      q = _rsQuery rspec
      ts = filter (q `matchesTransaction`) $ jtxns $ journalApplyValuationFromOpts rspec j
    printf "File %s: %d transactions\n" (journalFilePath j) (length ts)
