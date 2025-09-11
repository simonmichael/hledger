#!/usr/bin/env stack
--  stack runghc --package hledger
-------------------------------------------------------------------------------
-- About this script. You can remove this text. See also hledger-script-example-short.hs.
--
-- This is an example of a hledger addon command (an executable named hledger-*),
-- implemented as a haskell script which can use hledger's API.
-- Save it as a different name, give it execute permission, and customise it.
--
-- This script is run by stack. You can use any of the commands below:
--
-- The `stack runghc` command runs within a hledger source tree, using that local hledger version.
-- It installs haskell packages and GHC if needed, from the stackage snapshot configured in the stack.yaml file.
--  stack runghc --package hledger
--
-- The `stack script` command installs known hledger, haskell package and GHC versions from a specified stackage snapshot.
-- This is the most robust setup. --verbosity=info shows install progress. --compile makes a standalone binary.
--  stack script --snapshot lts-23.0                     --verbosity=error --package hledger
--  stack script --snapshot nightly-2024-12-16 --compile --verbosity=error --package hledger
--
-- Or the script can be run by cabal, which installs packages if needed (but not GHC).
--  #!/usr/bin/env cabal
--  {- cabal:
--  build-depends: base, directory, text, hledger
--  -}
--
-- If compiled to a binary, the script will run without stack or cabal or haskell packages.
-- (It will still require certain C libraries.)
-------------------------------------------------------------------------------
{-

script-example - my new script

-}

-- Haskell language customisations. OverloadedStrings is a useful one.
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/intro.html
{-# LANGUAGE OverloadedStrings #-}

-- Import things from haskell packages - the ones specified above, or included with the GHC specified above,
-- eg https://downloads.haskell.org/ghc/latest/docs/users_guide/9.10.1-notes.html#included-libraries
-- This provides most of hledger's API that's useful for scripts.
import Hledger.Cli.Script
-- Haskell has a built-in String type; hledger also uses the more efficient Text type.
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- If you use hledgerCommandMode, and getHledgerCliOpts below, your script will
-- support -h/--help, -f/--file and other common hledger options.
cmdmode = hledgerCommandMode (unlines
    ---------------------------standard terminal width-----------------------------
    -- command name on first line:
  ["script-example"
    -- then --help text:
  ,"Usage: hledger-script-example [OPTS] [ARGS]"
  ,"or:    hledger script-example -- [OPTS] [ARGS]"
  ,"Examples:"
  ,"$ hledger-script-example         # do the thing"
  ,"$ hledger-script-example --help  # print help"
  ])
    -- you can add or change options here (see CliOptions.hs); usually not needed
  [] [generalflagsgroup1] [] ([], Just $ argsFlag "[ARGS]")

-- Most scripts have three steps:
main = do

  -- 1. process command line options/arguments
  opts@CliOpts{reportspec_=rspec} <- getHledgerCliOpts cmdmode

  -- 2. read the journal file
  withJournal opts $ \j -> do

    -- 3. do something with it.
    putStrLn $ (show $ length $ jtxns j) <> " transactions in " <> (show $ journalFilePath j)



-------------------------------------------------------------------------------
{-
Script code examples. You can remove this text.

See also: bin/*.hs

Show a count of transactions, possibly filtered by a query:

    d <- getCurrentDay
    let
      q = _rsQuery rspec
      ts = filter (q `matchesTransaction`) $ jtxns $ journalApplyValuationFromOpts rspec j
    printf "File %s: %d transactions\n" (journalFilePath j) (length ts)

Run a postings report (like hledger register) and print the posting(s) with highest amount
(like hledger-register-max.hs):

    let
      postingReportItems = postingsReport rspec j
      maxbal = fifth5 $ maximumBy (comparing fifth5) r
      is = filter ((== maxbal).fifth5) r
    mapM_ printItem is

    printItem (_, _, _, p, bal) = do
      let
        d      = postingDate p
        mt     = ptransaction p
        desc   = fmt  30 $ maybe "-" tdescription mt
        acct   = fmt  30 $ paccount p
        amt    = fmta 12 $ T.pack $ showMixedAmountOneLine $ pamount p
        baltxt = fmta 12 $ T.pack $ showMixedAmountOneLine bal
      T.putStrLn $ T.unwords [showDate d, desc, "", acct, "", amt, " ", baltxt]
      where
        fmt w  = formatText True (Just w) (Just w) . textElideRight w
    fmta w = formatText False (Just w) Nothing

Use the [s|...|] multiline string syntax (requires --package string-qq and {-# LANGUAGE QuasiQuotes #-}):

    cmdmode = hledgerCommandMode (unlines
        -- Command name, then --help text. Note, empty help lines get stripped.
    [s| script-example
    This is an example of a (hledger-lib-using) hledger script."
    Usage: hledger-script-example [OPTS] [ARGS]"
    or:    hledger script-example -- [OPTS] [ARGS]"
    Save it under another name and customise it."
    The hledger- name makes it appear in hledger's commands list."
    Examples:"
    $ hledger-script-example --help"
    (this help)"
      |]
        ------------------------------------78----------------------------------------
      [] [generalflagsgroup1] [] ([], Just $ argsFlag "[ARGS]")  -- or Nothing

-}
