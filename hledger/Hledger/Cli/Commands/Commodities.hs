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
import Hledger.Cli.Utils (printTitle)
import Data.List.Extra (nubSort)
import Data.List ((\\))


-- | Command line options for this command.
commoditiesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Commodities.txt")
  [flagNone ["used"]         (setboolopt "used")       "list commodities used"
  ,flagNone ["declared"]     (setboolopt "declared")   "list commodities declared"
  ,flagNone ["undeclared"]   (setboolopt "undeclared") "list commodities used but not declared"
  ,flagNone ["unused"]       (setboolopt "unused")     "list commodities declared but not used"
  ,flagNone ["find"]         (setboolopt "find")       "list the first commodity matched by the first argument (a case-insensitive infix regexp)"
  ]
  [generalflagsgroup2]
  confflags
  ([], Just $ argsFlag "[QUERY..]")

commodities :: CliOpts -> Journal -> IO ()
commodities opts@CliOpts{rawopts_=rawopts, reportspec_=ReportSpec{_rsQuery=query, _rsReportOpts=ropts}} j = do
  printTitle ropts
  let
    -- A date query (or report period) restricts results to commodities
    -- arising in that period - those used in matching transactions or P
    -- directives. Dateless sources (commodity directives) are excluded.
    -- --declared and --unused are whole-journal hygiene checks and
    -- ignore the date query. --undeclared narrows only its "used" side
    -- (commodities used in the period but not declared anywhere).
    -- Other query terms (cur:, tag:, ...) continue to filter the final
    -- commodity name list.
    dateq    = filterQuery queryIsDate query
    nondateq = filterQuery (not . queryIsDate) query
    hasdateq = not $ queryIsNull dateq
    jmatched = j { jtxns = dbg5With (\xs -> "txnsmatched: " <> show (length xs))
                      $ filter (matchesTransaction    dateq) (jtxns j)
                 , jpricedirectives = dbg5With (\xs -> "pricedirectivesmatched: " <> show (length xs))
                      $ filter (matchesPriceDirective dateq) (jpricedirectives j)
                 }
    -- All commodity names in a journal (declarations, P directives, and posting amounts).
    commoditiesIn jj = nubSort $ concat [
       journalCommoditiesDeclared jj
      ,map pdcommodity $ jpricedirectives jj          -- first symbol from P directives
      ,map acommodity (S.toList $ journalAmounts jj)  -- includes the second symbol from P directives
      ]
    filt = filter (matchesCommodityExtra (journalCommodityTags j) nondateq)
    usedmatched = dbg5 "usedmatched" $ S.toList $ journalCommoditiesFromPriceDirectives jmatched <> journalCommoditiesFromTransactions jmatched
    usedall     = dbg5 "usedall"     $ S.toList $ journalCommoditiesFromPriceDirectives j        <> journalCommoditiesFromTransactions j
    declared'   = dbg5 "declared"    $ M.keys $ jdeclaredcommodities j
    unused      = dbg5 "unused"      $ declared'   \\ usedall      -- whole-journal hygiene; date ignored
    undeclared  = dbg5 "undeclared"  $ usedmatched \\ declared'    -- date narrows the used side
    all'        = dbg5 "all"         $ if hasdateq then usedmatched else commoditiesIn j
    -- --find is a name lookup over all commodity names; date queries don't apply.
    found       = dbg5 "found"      $ findMatchedByArgument rawopts "commodity" (commoditiesIn j)

  mapM_ T.putStrLn $
    case declarablesSelectorFromOpts opts of
      Nothing         -> filt all'
      Just Used       -> filt usedmatched
      Just Declared   -> filt declared'
      Just Undeclared -> filt undeclared
      Just Unused     -> filt unused
      Just Find       -> [found]
