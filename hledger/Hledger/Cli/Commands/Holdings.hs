{-|

The @holdings@ command shows a report of investment holdings (lot-tracked assets).

Currently it shows a mockup of the planned layout, with sample data.
See doc/SPEC-holdings.md.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Holdings (
  holdingsmode
 ,holdings
) where

import Data.Default (def)
import Data.Text (Text)
import Data.Text.Lazy.IO qualified as TL

import Hledger
import Hledger.Cli.CliOptions
import Text.Tabular.AsciiWide

-- | Command line options for this command.
holdingsmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Holdings.txt")
  (flattreeflags True)
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | Show the holdings report.
-- Phase 1: shows a hardcoded mockup of the planned layout, ignoring the journal.
-- The --lots and --tree flags select the corresponding layout variant.
holdings :: CliOpts -> Journal -> IO ()
holdings CliOpts{rawopts_=rawopts, reportspec_=ReportSpec{_rsReportOpts=ropts}} _j = do
  putStrLn "Holdings on 2026-03-31 (mockup with sample data)"
  putStrLn ""
  TL.putStrLn $ renderTable
    def{tableBorders=False}
    (textCell TopLeft)
    (textCell TopRight)
    (textCell TopRight)
    tbl
  where
    tbl | lots && tree = treeMockup
        | lots         = lotsMockup
        | otherwise    = defaultMockup
      where
        lots = boolopt "lots" rawopts
        tree = accountlistmode_ ropts == ALTree

-- | Build a mockup holdings table from column headings, rows of
-- (account name, cells), and a possible totals row.
mockupTable :: [Text] -> [(Text, [Text])] -> Maybe [Text] -> Table Text Text Text
mockupTable colheadings rows mtotalrow = maybe maintbl addtotal mtotalrow
  where
    maintbl = Table
      (Group NoLine $ map (Header . fst) rows)
      (Group NoLine $ map Header colheadings)
      (map snd rows)
    addtotal totalrow = concatTables SingleLine maintbl $
      Table (Group NoLine [Header ""]) (Header []) [totalrow]

-- Sample data: two AAPL buys in assets:broker:stocks, one MSFT buy in
-- assets:broker:funds, a FIFO sale of 5 AAPL, and market prices
-- (AAPL $72, MSFT $410) on the report date 2026-03-31.

-- | Default layout: list mode, lot subaccounts hidden.
defaultMockup :: Table Text Text Text
defaultMockup = mockupTable
  ["Date", "Age", "Quantity", "Avg cost", "Cost", "Price", "Value", "Gain"]
  [ ("assets:broker:funds",  ["2026-02-15", "44d", "5 MSFT",  "$400.00", "$2000", "$410", "$2050", "$50 (+2.5%)"])
  , ("assets:broker:stocks", ["",           "",    "15 AAPL", "$56.67",  "$850",  "$72",  "$1080", "$230 (+27.1%)"])
  ]
  (Just ["", "", "", "", "$2850", "", "$3130", "$280 (+9.8%)"])

-- | With --lots: lot subaccounts become rows.
lotsMockup :: Table Text Text Text
lotsMockup = mockupTable
  ["Date", "Age", "Quantity", "Unit cost", "Cost", "Price", "Value", "Gain"]
  [ ("assets:broker:funds:{2026-02-15, $400}", ["2026-02-15", "44d", "5 MSFT",  "$400", "$2000", "$410", "$2050", "$50 (+2.5%)"])
  , ("assets:broker:stocks:{2026-01-15, $50}", ["2026-01-15", "75d", "5 AAPL",  "$50",  "$250",  "$72",  "$360",  "$110 (+44.0%)"])
  , ("assets:broker:stocks:{2026-02-01, $60}", ["2026-02-01", "58d", "10 AAPL", "$60",  "$600",  "$72",  "$720",  "$120 (+20.0%)"])
  ]
  (Just ["", "", "", "", "$2850", "", "$3130", "$280 (+9.8%)"])

-- | With --lots --tree: parent rows aggregate their subaccounts.
treeMockup :: Table Text Text Text
treeMockup = mockupTable
  ["Date", "Age", "Quantity", "Unit cost", "Cost", "Price", "Value", "Gain"]
  [ ("assets",                   ["",           "",    "15 AAPL\n5 MSFT", "",        "$2850", "",     "$3130", "$280 (+9.8%)"])
  , ("  broker",                 ["",           "",    "15 AAPL\n5 MSFT", "",        "$2850", "",     "$3130", "$280 (+9.8%)"])
  , ("    funds",                ["2026-02-15", "44d", "5 MSFT",          "$400",    "$2000", "$410", "$2050", "$50 (+2.5%)"])
  , ("      {2026-02-15, $400}", ["2026-02-15", "44d", "5 MSFT",          "$400",    "$2000", "$410", "$2050", "$50 (+2.5%)"])
  , ("    stocks",               ["",           "",    "15 AAPL",         "$56.67",  "$850",  "$72",  "$1080", "$230 (+27.1%)"])
  , ("      {2026-01-15, $50}",  ["2026-01-15", "75d", "5 AAPL",          "$50",     "$250",  "$72",  "$360",  "$110 (+44.0%)"])
  , ("      {2026-02-01, $60}",  ["2026-02-01", "58d", "10 AAPL",         "$60",     "$600",  "$72",  "$720",  "$120 (+20.0%)"])
  ]
  Nothing
