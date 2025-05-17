{-|

The @payees@ command lists allpayees seen in transactions.

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
import Data.List.Extra (nubSortBy)
import qualified Data.Text.Collate as Collate
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
  d <- getCurrentDay
  let q  = queryFromOpts d ropts
      ts = entriesReport ropts q j
      collator = Collate.collatorFor "en" (Collate.CollatorOptions {Collate.strength = Collate.Primary})
      payees = nubSortBy (Collate.compare collator) $ map transactionPayee ts

  mapM_ T.putStrLn payees
