{-|

The @descriptions@ command lists all unique descriptions seen in transactions, sorted alphabetically.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Descriptions (
  descriptionsmode
 ,descriptions
) where

import Data.List.Extra (nubSort)
import qualified Data.Text.IO as T

import Hledger
import Hledger.Cli.CliOptions


-- | Command line options for this command.
descriptionsmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Descriptions.txt")
  []
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | The descriptions command.
descriptions :: CliOpts -> Journal -> IO ()
descriptions CliOpts{reportspec_=rspec} j = do
  let ts = entriesReport rspec j
      descriptions = nubSort $ map tdescription ts

  mapM_ T.putStrLn descriptions
