{-|

The @commodities@ command lists commodity/currency symbols.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Commodities (
  commoditiesmode
 ,commodities
) where

import Control.Monad
import Data.List
import qualified Data.Map as M
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
commodities _copts j = do
  let cs = filter (/= "AUTO") $
           nub $ sort $ M.keys (jcommodities j) ++ M.keys (jinferredcommodities j)
  forM_ cs T.putStrLn
