{-|

The @commodities@ command lists commodity/currency symbols.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Commodities (
  commoditiesmode
 ,commodities
) where

import qualified Data.Set as S
import qualified Data.Text.IO as T

import Hledger
import Hledger.Cli.CliOptions


-- | Command line options for this command.
commoditiesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Commodities.txt")
  []
  [generalflagsgroup2]
  []
  ([], Nothing)

commodities :: CliOpts -> Journal -> IO ()
commodities _copts =
  mapM_ T.putStrLn . S.filter (/= "AUTO") . journalCommodities
