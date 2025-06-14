{-|

The @payees@ command lists all unique payees (description part before a |) seen in transactions, sorted alphabetically.

-}

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Hledger.Cli.Commands.Payees (
  payeesmode
 ,payees
) where

import qualified Data.Text.IO as T
import System.Console.CmdArgs.Explicit

import Hledger
import Hledger.Cli.CliOptions
import Data.List ((\\))
import Data.List.Extra (nubSort)


-- | Command line options for this command.
payeesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Payees.txt")
  [flagNone ["used"]         (setboolopt "used")       "list payees used"
  ,flagNone ["declared"]     (setboolopt "declared")   "list payees declared"
  ,flagNone ["undeclared"]   (setboolopt "undeclared") "list payees used but not declared"
  ,flagNone ["unused"]       (setboolopt "unused")     "list payees declared but not used"
  ]
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[QUERY..]")

-- | The payees command.
payees :: CliOpts -> Journal -> IO ()
payees opts@CliOpts{reportspec_=ReportSpec{_rsQuery=query}} j = do
  let
    -- XXX matchesPayeeWIP is currently an alias for matchesDescription, not sure if it matters
    matchedused       = dbg5 "matchedused"       $ nubSort $ map transactionPayee $ filter (matchesTransaction query) $ jtxns j
    matcheddeclared   = dbg5 "matcheddeclared"   $ nubSort $ filter (matchesPayeeWIP query) $ journalPayeesDeclared j
    matchedunused     = dbg5 "matchedunused"     $ nubSort $ matcheddeclared \\ matchedused
    matchedundeclared = dbg5 "matchedundeclared" $ nubSort $ matchedused     \\ matcheddeclared
    matchedall        = dbg5 "matchedall"        $ nubSort $ matcheddeclared ++ matchedused
  mapM_ T.putStrLn $ case usedOrDeclaredFromOpts opts of
    Nothing         -> matchedall
    Just Used       -> matchedused
    Just Declared   -> matcheddeclared
    Just Undeclared -> matchedundeclared
    Just Unused     -> matchedunused

