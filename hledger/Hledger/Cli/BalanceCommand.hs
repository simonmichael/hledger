{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-|

Common helpers for making compound balance-report-ish commands like
balancesheet, cashflow, or incomestatement.

-}

module Hledger.Cli.BalanceCommand (
  BalanceCommandSpec(..)
 ,balanceCommandMode
 ,balanceCommand
) where

import Control.Monad (unless)
import Data.List (intercalate, foldl', isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..), (<>))
import System.Console.CmdArgs.Explicit as C
import Text.Tabular as T

import Hledger
import Hledger.Cli.Balance
import Hledger.Cli.CliOptions

-- | Description of a compound balance-report-like command, consisting of
-- multiple named subreports displayed in order and then totalled.
data BalanceCommandSpec = BalanceCommandSpec {
  bcname     :: String,                        -- ^ command name
  bcaliases  :: [String],                      -- ^ command aliases
  bchelp     :: String,                        -- ^ command line help
  bctitle    :: String,                        -- ^ overall report title
  bcqueries  :: [(String, Journal -> Query)],  -- ^ title and (journal-parameterised) query for each subreport
  bctype     :: BalanceType                    -- ^ the type of "balance" this report shows (overrides command line flags)
}

-- | Generate a cmdargs option-parsing mode from a compound balance command 
-- specification.
balanceCommandMode :: BalanceCommandSpec -> Mode RawOpts
balanceCommandMode BalanceCommandSpec{..} = (defCommandMode $ bcname : bcaliases) {
  modeHelp = bchelp `withAliases` bcaliases
 ,modeGroupFlags = C.Group {
     groupUnnamed = [
      flagNone ["change"] (\opts -> setboolopt "change" opts)
        ("show balance change in each period" ++ defType PeriodChange)
     ,flagNone ["cumulative"] (\opts -> setboolopt "cumulative" opts)
        ("show balance change accumulated across periods (in multicolumn reports)"
            ++ defType CumulativeChange
        )
     ,flagNone ["historical","H"] (\opts -> setboolopt "historical" opts)
        ("show historical ending balance in each period (includes postings before report start date)"
            ++ defType HistoricalBalance
        )
     ,flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show accounts as a list"
     ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
     ,flagNone ["no-total","N"] (\opts -> setboolopt "no-total" opts) "omit the final total row"
     ,flagNone ["tree"] (\opts -> setboolopt "tree" opts) "show accounts as a tree; amounts include subaccounts (default in simple reports)"
     ,flagNone ["average","A"] (\opts -> setboolopt "average" opts) "show a row average column (in multicolumn reports)"
     ,flagNone ["row-total","T"] (\opts -> setboolopt "row-total" opts) "show a row total column (in multicolumn reports)"
     ,flagNone ["no-elide"] (\opts -> setboolopt "no-elide" opts) "don't squash boring parent accounts (in tree mode)"
     ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "use this custom line format (in simple reports)"
     ,flagNone ["pretty-tables"] (\opts -> setboolopt "pretty-tables" opts) "use unicode when displaying tables"
     ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
 where
   defType :: BalanceType -> String
   defType bt | bt == bctype = " (default)"
              | otherwise    = ""

-- | Generate a runnable command from a compound balance command specification.
balanceCommand :: BalanceCommandSpec -> (CliOpts -> Journal -> IO ())
balanceCommand BalanceCommandSpec{..} CliOpts{command_=cmd, reportopts_=ropts, rawopts_=rawopts} j = do
    d <- getCurrentDay
    let
      -- use the default balance type for this report, unless the user overrides  
      mBalanceTypeOverride =
        case reverse $ filter (`elem` ["change","cumulative","historical"]) $ map fst rawopts of
          "historical":_ -> Just HistoricalBalance
          "cumulative":_ -> Just CumulativeChange
          "change":_     -> Just PeriodChange
          _              -> Nothing
      balancetype = fromMaybe bctype mBalanceTypeOverride
      -- when user overrides, add an indication to the report title
      title = bctitle ++ maybe "" (' ':) mtitleclarification
        where
          mtitleclarification = flip fmap mBalanceTypeOverride $ \t ->
            case t of
              PeriodChange      -> "(Balance Changes)"
              CumulativeChange  -> "(Cumulative Ending Balances)"
              HistoricalBalance -> "(Historical Ending Balances)"
      -- set balance type and possibly tree mode in the report options. Some older notes: 
      -- For --historical/--cumulative, we must use multiBalanceReport (this forces --no-elide).
      -- These settings format the output in a way that we can convert to a normal balance report 
      -- using singleBalanceReport, see Balance.hs for more information.
      ropts'
        | not (flat_ ropts) && 
          interval_ ropts==NoInterval && 
          balancetype `elem` [CumulativeChange, HistoricalBalance]
            = ropts{balancetype_=balancetype, accountlistmode_=ALTree}
        | otherwise
            = ropts{balancetype_=balancetype}
      userq = queryFromOpts d ropts'

    case interval_ ropts' of

      NoInterval -> do
        -- single-column report
        let (subreportstr, total) = foldMap (uncurry (balanceCommandSingleColumnReport ropts' userq j)) bcqueries
        putStrLn $ title ++ "\n"
        mapM_ putStrLn subreportstr
        unless (no_total_ ropts' || cmd=="cashflow") . mapM_ putStrLn $ -- TODO temp
          [ "Total:"
          , "--------------------"
          , padLeftWide 20 $ showamt (getSum total)
          ]
        where
          showamt | color_ ropts' = cshowMixedAmountWithoutPrice
                  | otherwise    = showMixedAmountWithoutPrice
          
      _ -> do
        -- multi-column report
        let
          (subreporttables, subreporttotals, Sum overalltotal) = foldMap (uncurry (balanceCommandMultiColumnReports ropts' userq j)) bcqueries
          overalltable = case subreporttables of
            t1:ts -> foldl' concatTables t1 ts
            []    -> T.empty
          overalltable'
            | no_total_ ropts' || length bcqueries == 1 =
                overalltable
            | otherwise =
                overalltable
                +====+
                row "Total"
                    (overalltotals ++ (if row_total_ ropts' && not (null overalltotals) then [overalltotal]   else [])
                                   ++ (if average_ ropts'   && not (null overalltotals) then [overallaverage] else [])
                    )
              where
                overalltotals = case subreporttotals of
                  a1:as -> foldl' (zipWith (+)) a1 as
                  []    -> []
                overallaverage = overalltotal `divideMixedAmount` fromIntegral (length overalltotals)
        putStrLn title
        putStrLn $ renderBalanceReportTable ropts' overalltable'

-- Add the second table below the first, discarding its column headings.
concatTables (Table hLeft hTop dat) (Table hLeft' _ dat') =
    Table (T.Group DoubleLine [hLeft, hLeft']) hTop (dat ++ dat')

-- | Run one subreport for a single-column compound balance command.
-- Currently this returns the plain text rendering of the subreport,
-- and its total.
balanceCommandSingleColumnReport
    :: ReportOpts
    -> Query
    -> Journal
    -> String
    -> (Journal -> Query)
    -> ([String], Sum MixedAmount)
balanceCommandSingleColumnReport ropts q0 j t q = ([view], Sum amt)
    where
      q' = And [q0, q j]
      rep@(_ , amt)
        -- For --historical/--cumulative, we must use multiBalanceReport.
        -- (This forces --no-elide.)
        -- See Balance.hs's implementation of 'balance' for more information
        | balancetype_ ropts `elem` [HistoricalBalance, CumulativeChange]
            = singleBalanceReport ropts q' j
        | otherwise
            = balanceReport ropts q' j
      view = intercalate "\n" [t <> ":", balanceReportAsText ropts rep]

-- | Run all the subreports for a multi-column compound balance command.
-- Currently this returns a table of rendered balance amounts for each 
-- subreport (including a totals row), the totals row for each subreport 
-- (again, as mixedamounts), and the grand total.
balanceCommandMultiColumnReports
    :: ReportOpts
    -> Query
    -> Journal
    -> String
    -> (Journal -> Query)
    -> ([Table String String MixedAmount], [[MixedAmount]], Sum MixedAmount)
balanceCommandMultiColumnReports ropts q0 j t q = ([tabl], [coltotals], Sum tot)
    where
      singlesection = "Cash" `isPrefixOf` t -- TODO temp
      ropts' = ropts { no_total_ = singlesection && no_total_ ropts, empty_ = True }
      q' = And [q0, q j]
      MultiBalanceReport (dates, rows, (coltotals,tot,avg)) =
          multiBalanceReport ropts' q' j
      rows' | empty_ ropts = rows
            | otherwise    = filter (not . emptyRow) rows
        where
          emptyRow (_,_,_,amts,_,_) = all isZeroMixedAmount amts
      r = MultiBalanceReport (dates, rows', (coltotals, tot, avg))
      Table hLeft hTop dat = balanceReportAsTable ropts' r
      tabl = Table (T.Group SingleLine [Header t, hLeft]) hTop ([]:dat)

