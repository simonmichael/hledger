{-|

The @payees@ command lists all unique payees (description part before a |) seen in transactions, sorted alphabetically.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Hledger.Cli.Commands.Payees (
  payeesmode
 ,payees
) where

import Data.List.Extra (nubSort)
import qualified Data.Text.IO as T

import Hledger
import Hledger.Cli.CliOptions


-- | Command line options for this command.
payeesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Payees.txt")
  []
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | The payees command.
payees :: CliOpts -> Journal -> IO ()
payees CliOpts{reportopts_=ropts} j = do
  let ts = entriesReport ropts j
      payees = nubSort $ map transactionPayee ts
  mapM_ T.putStrLn payees
