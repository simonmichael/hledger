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
import System.Console.CmdArgs.Explicit as C

import Hledger
import Hledger.Cli.CliOptions


-- | Command line options for this command.
payeesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Payees.txt")
  [flagNone ["declared"] (setboolopt "declared") "show payees declared with payee directives"
  ,flagNone ["used"] (setboolopt "used") "show payees referenced by transactions"
  ]
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | The payees command.
payees :: CliOpts -> Journal -> IO ()
payees CliOpts{rawopts_=rawopts, reportspec_=ReportSpec{rsQuery=query}} j = do
  let 
    declared = boolopt "declared" rawopts
    used     = boolopt "used"     rawopts
    -- XXX matchesPayee is currently an alias for matchesDescription, not sure if it matters
    matcheddeclaredpayees = filter (matchesPayeeWIP query) $ journalPayeesDeclared j
    matchedusedpayees     = map transactionPayee $ filter (matchesTransaction query) $ jtxns j
    payees = nubSort $
      if | declared     && not used -> matcheddeclaredpayees
         | not declared && used     -> matchedusedpayees
         | otherwise                -> matcheddeclaredpayees ++ matchedusedpayees
  mapM_ T.putStrLn payees
