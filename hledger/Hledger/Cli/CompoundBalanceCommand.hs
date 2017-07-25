{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-|

Common helpers for making multi-section balance report commands 
like balancesheet, cashflow, and incomestatement.

-}

module Hledger.Cli.CompoundBalanceCommand (
  CompoundBalanceCommandSpec(..)
 ,compoundBalanceCommandMode
 ,compoundBalanceCommand
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

-- | Description of a compound balance report command, 
-- from which we generate the command's cmdargs mode and IO action.
-- A compound balance report shows one or more sections/subreports, 
-- each with its own title and subtotals row, in a certain order, 
-- plus a grand totals row if there's more than one section.
-- Examples are the balancesheet, cashflow and incomestatement commands.
data CompoundBalanceCommandSpec = CompoundBalanceCommandSpec {
  cbcname     :: String,                        -- ^ command name
  cbcaliases  :: [String],                      -- ^ command aliases
  cbchelp     :: String,                        -- ^ command line help
  cbctitle    :: String,                        -- ^ overall report title
  cbcqueries  :: [(String, Journal -> Query)],  -- ^ title and (journal-parameterised) query for each subreport
  cbctype     :: BalanceType                    -- ^ the type of "balance" this report shows (overrides command line flags)
}

-- | Generate a cmdargs option-parsing mode from a compound balance command 
-- specification.
compoundBalanceCommandMode :: CompoundBalanceCommandSpec -> Mode RawOpts
compoundBalanceCommandMode CompoundBalanceCommandSpec{..} = (defCommandMode $ cbcname : cbcaliases) {
  modeHelp = cbchelp `withAliases` cbcaliases
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
   defType bt | bt == cbctype = " (default)"
              | otherwise    = ""

-- | Generate a runnable command from a compound balance command specification.
compoundBalanceCommand :: CompoundBalanceCommandSpec -> (CliOpts -> Journal -> IO ())
compoundBalanceCommand CompoundBalanceCommandSpec{..} CliOpts{command_=cmd, reportopts_=ropts, rawopts_=rawopts} j = do
    d <- getCurrentDay
    let
      -- use the default balance type for this report, unless the user overrides  
      mBalanceTypeOverride =
        case reverse $ filter (`elem` ["change","cumulative","historical"]) $ map fst rawopts of
          "historical":_ -> Just HistoricalBalance
          "cumulative":_ -> Just CumulativeChange
          "change":_     -> Just PeriodChange
          _              -> Nothing
      balancetype = fromMaybe cbctype mBalanceTypeOverride
      -- when user overrides, add an indication to the report title
      title = cbctitle ++ maybe "" (' ':) mtitleclarification
        where
          mtitleclarification = flip fmap mBalanceTypeOverride $ \t ->
            case t of
              PeriodChange      -> "(Balance Changes)"
              CumulativeChange  -> "(Cumulative Ending Balances)"
              HistoricalBalance -> "(Historical Ending Balances)"
      -- Set balance type in the report options.
      -- XXX Also, use tree mode (by default, at least?) if --cumulative/--historical 
      -- are used in single column mode, since in that situation we will be using 
      -- singleBalanceReport which does not support eliding boring parents,
      -- and tree mode hides this.. or something.. 
      -- see also compoundBalanceCommandSingleColumnReport, #565  
      ropts'
        | not (flat_ ropts) && 
          interval_ ropts==NoInterval && 
          balancetype `elem` [CumulativeChange, HistoricalBalance]
            = ropts{balancetype_=balancetype, accountlistmode_=ALTree}
        | otherwise
            = ropts{balancetype_=balancetype}
      userq = queryFromOpts d ropts'

    case interval_ ropts' of

      -- single-column report
      NoInterval -> do
        let
          -- concatenate the rendering and sum the totals from each subreport
          (subreportstr, total) = 
            foldMap (uncurry (compoundBalanceCommandSingleColumnReport ropts' userq j)) cbcqueries
        putStrLn $ title ++ "\n"
        mapM_ putStrLn subreportstr
        unless (no_total_ ropts' || cmd=="cashflow") . mapM_ putStrLn $
          [ "Total:"
          , "--------------------"
          , padLeftWide 20 $ showamt (getSum total)
          , ""
          ]
        where
          showamt | color_ ropts' = cshowMixedAmountWithoutPrice
                  | otherwise    = showMixedAmountWithoutPrice
          
      -- multi-column report
      _ -> do
        let
          -- list the tables, list the totals rows, and sum the totals from each subreport
          (subreporttables, subreporttotals, Sum overalltotal) = 
            foldMap (uncurry (compoundBalanceCommandMultiColumnReport ropts' userq j)) cbcqueries
          overalltable = case subreporttables of
            t1:ts -> foldl' concatTables t1 ts
            []    -> T.empty
          overalltable'
            | no_total_ ropts' || length cbcqueries == 1 =
                overalltable
            | otherwise =
                overalltable
                +====+
                row "Total" overalltotals'
              where
                overalltotals = case subreporttotals of
                  [] -> []
                  ts ->
                    -- Sum the subtotals in each column.
                    -- A subreport might be empty and have no subtotals, count those as zeros (#588).
                    -- Short subtotals rows are also implicitly padded with zeros, though that is not expected to happen.  
                    let
                      numcols = maximum $ map length ts
                      zeros = replicate numcols nullmixedamt
                      ts' = [take numcols $ as ++ repeat nullmixedamt | as <- ts]
                    in foldl' (zipWith (+)) zeros ts'
                -- add values for the total/average columns if enabled
                overalltotals'
                  | null overalltotals = []
                  | otherwise =
                      overalltotals
                      ++ (if row_total_ ropts' then [overalltotal]   else [])
                      ++ (if average_ ropts'   then [overallaverage] else [])
                      where
                        overallaverage = 
                          overalltotal `divideMixedAmount` fromIntegral (length overalltotals) -- depends on non-null overalltotals
        putStrLn $ title ++ "\n"
        putStr $ renderBalanceReportTable ropts' overalltable'

-- Add the second table below the first, discarding its column headings.
concatTables (Table hLeft hTop dat) (Table hLeft' _ dat') =
    Table (T.Group DoubleLine [hLeft, hLeft']) hTop (dat ++ dat')

-- | Run one subreport for a compound balance command in single-column mode.
-- Currently this returns the plain text rendering of the subreport, and its total.
-- The latter is wrapped in a Sum for easy monoidal combining.
compoundBalanceCommandSingleColumnReport
    :: ReportOpts
    -> Query
    -> Journal
    -> String
    -> (Journal -> Query)
    -> ([String], Sum MixedAmount)
compoundBalanceCommandSingleColumnReport ropts userq j subreporttitle subreportqfn = 
  ([subreportstr], Sum total)
  where
    q = And [subreportqfn j, userq]
    r@(_,total)
      -- XXX For --historical/--cumulative, we must use singleBalanceReport;
      -- otherwise we use balanceReport -- because it supports eliding boring parents. 
      -- See also compoundBalanceCommand, Balance.hs -> balance.
      | balancetype_ ropts `elem` [CumulativeChange, HistoricalBalance] = singleBalanceReport ropts q j
      | otherwise                                                       = balanceReport       ropts q j
    subreportstr = intercalate "\n" [subreporttitle <> ":", balanceReportAsText ropts r]

-- | Run one subreport for a compound balance command in multi-column mode.
-- Currently this returns a table of rendered balance amounts, including the
-- totals row; the totals row again, as mixedamounts; and the grand total.
-- The first two are wrapped in a list and the third in a Sum, for easy
-- monoidal combining.
compoundBalanceCommandMultiColumnReport
    :: ReportOpts
    -> Query
    -> Journal
    -> String
    -> (Journal -> Query)
    -> ([Table String String MixedAmount], [[MixedAmount]], Sum MixedAmount)
compoundBalanceCommandMultiColumnReport ropts userq j subreporttitle subreportqfn =
  ([tabl], [coltotals], Sum tot)
  where
    -- disable totals row if there's just one section and --no-total
    -- force --empty to ensure same columns in all sections, or something
    ropts' = ropts { no_total_ = singlesection && no_total_ ropts, empty_ = True }
      where
        singlesection = "Cash" `isPrefixOf` subreporttitle -- TODO temp
    -- run the report
    q = And [subreportqfn j, userq]
    MultiBalanceReport (dates, rows, (coltotals,tot,avg)) = multiBalanceReport ropts' q j
    -- if user didn't specify --empty, now remove the all-zero rows
    r = MultiBalanceReport (dates, rows', (coltotals, tot, avg))
      where
        rows' | empty_ ropts = rows
              | otherwise    = filter (not . emptyRow) rows
          where
            emptyRow (_,_,_,amts,_,_) = all isZeroMixedAmount amts
    -- convert to a table for rendering
    Table lefthdrs tophdrs cells = balanceReportAsTable ropts' r
    -- tweak the table layout
    tabl = Table (T.Group SingleLine [Header subreporttitle, lefthdrs]) tophdrs ([]:cells)
