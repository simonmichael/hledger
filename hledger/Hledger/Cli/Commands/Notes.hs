{-|

The @notes@ command lists allpayees seen in transactions.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Hledger.Cli.Commands.Notes (
  notesmode
 ,notes
) where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.List.Extra (nubSortBy)
import qualified Data.Text.ICU as T
import qualified Data.Text.IO as T

import Hledger
import Hledger.Cli.CliOptions


-- | Command line options for this command.
notesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Notes.txt")
  []
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | The notes command.
notes :: CliOpts -> Journal -> IO ()
notes CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  let q  = queryFromOpts d ropts
      ts = entriesReport ropts q j
      notes = nubSortBy (T.compare [T.CompareIgnoreCase]) $ map transactionNote ts

  mapM_ T.putStrLn notes
