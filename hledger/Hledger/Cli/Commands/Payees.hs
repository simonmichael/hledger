{-|

The @payees@ command lists payees:

- with the notes option the note field is included along with payees

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

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Function
import Data.List
import qualified Data.Text.IO as T
import System.Console.CmdArgs.Explicit as C

import Hledger
import Hledger.Cli.CliOptions


-- | Command line options for this command.
payeesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Payees.txt")
  [flagNone ["notes"] (setboolopt "notes") "include note field with payees"
  ]
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | The payees command.
payees :: CliOpts -> Journal -> IO ()
payees CliOpts{rawopts_=rawopts, reportopts_=ropts} j = do
  d <- getCurrentDay
  let shownotes = boolopt "notes" rawopts
      q  = queryFromOpts d ropts
      ts = entriesReport ropts q j
      payees = nub $ sort $ map (if shownotes then tdescription else transactionPayee) ts

  mapM_ T.putStrLn payees
