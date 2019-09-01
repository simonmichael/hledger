{-|

The @descriptions@ command lists allpayees seen in transactions.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Descriptions (
  descriptionsmode
 ,descriptions
) where

import Data.List
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
descriptions CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  let q  = queryFromOpts d ropts
      ts = entriesReport ropts q j
      descriptions = nub $ sort $ map tdescription ts

  mapM_ T.putStrLn descriptions
