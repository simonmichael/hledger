{-|

The @commodities@ command lists commodity/currency symbols.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Commodities (
  commoditiesmode
 ,commodities
) where

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text.IO qualified as T
import System.Console.CmdArgs.Explicit

import Hledger
import Hledger.Cli.CliOptions
import Data.List.Extra (nubSort)
import Data.List ((\\))


-- | Command line options for this command.
commoditiesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Commodities.txt")
  [flagNone ["used"]         (setboolopt "used")       "list commodities used"
  ,flagNone ["declared"]     (setboolopt "declared")   "list commodities declared"
  ,flagNone ["undeclared"]   (setboolopt "undeclared") "list commodities used but not declared"
  ,flagNone ["unused"]       (setboolopt "unused")     "list commodities declared but not used"
  ]
  [generalflagsgroup2]
  confflags
  ([], Just $ argsFlag "[QUERY..]")

commodities :: CliOpts -> Journal -> IO ()
commodities opts@CliOpts{reportspec_ = ReportSpec{_rsQuery = query}} j = do
  let
    used       = dbg5 "used"       $
      S.toList $ journalCommoditiesFromPriceDirectives j <> journalCommoditiesFromTransactions j
    declared'  = dbg5 "declared"   $ M.keys $ jdeclaredcommodities j
    unused     = dbg5 "unused"     $ declared' \\ used
    undeclared = dbg5 "undeclared" $ used     \\ declared'
    all'       = dbg5 "all"        $ nubSort $ concat [
       journalCommoditiesDeclared j
      ,map pdcommodity $ jpricedirectives j          -- gets the first symbol from P directives
      ,map acommodity (S.toList $ journalAmounts j)  -- includes the second symbol from P directives
      ]

  mapM_ T.putStrLn $ filter (matchesCommodity query) $
    case usedOrDeclaredFromOpts opts of
      Nothing         -> all'
      Just Used       -> used
      Just Declared   -> declared'
      Just Undeclared -> undeclared
      Just Unused     -> unused
