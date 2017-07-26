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

import Data.List (intercalate, foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..), (<>))
import System.Console.CmdArgs.Explicit as C
import Text.CSV
import Text.Tabular as T

import Hledger
import Hledger.Cli.Balance
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils (writeOutput)

-- | Description of a compound balance report command, 
-- from which we generate the command's cmdargs mode and IO action.
-- A compound balance report command shows one or more sections/subreports, 
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
     ,outputFormatFlag
     ,outputFileFlag
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
compoundBalanceCommand CompoundBalanceCommandSpec{..} opts@CliOpts{command_=cmd, reportopts_=ropts, rawopts_=rawopts} j = do
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
      format = outputFormatFromOpts opts

    case interval_ ropts' of

      -- single-column report
      -- TODO refactor, support output format like multi column 
      NoInterval -> do
        let
          -- concatenate the rendering and sum the totals from each subreport
          (subreportstr, total) = 
            foldMap (uncurry (compoundBalanceCommandSingleColumnReport ropts' userq j)) cbcqueries

        writeOutput opts $ unlines $
          [title ++ "\n"] ++
          subreportstr ++
          if (no_total_ ropts' || cmd=="cashflow")
           then []
           else
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
          -- make a CompoundBalanceReport
          namedsubreports = 
            map (\(subreporttitle, subreportq) -> 
                  (subreporttitle, compoundBalanceSubreport ropts' userq j subreportq)) 
                cbcqueries
          subtotalrows = [coltotals | MultiBalanceReport (_,_,(coltotals,_,_)) <- map snd namedsubreports]
          overalltotals = case subtotalrows of
            [] -> ([], nullmixedamt, nullmixedamt)
            rs ->
              -- Sum the subtotals in each column.
              -- A subreport might be empty and have no subtotals, count those as zeros (#588).
              -- Short subtotals rows are also implicitly padded with zeros, though that is not expected to happen.  
              let
                numcols = maximum $ map length rs  -- depends on non-null ts
                zeros = replicate numcols nullmixedamt
                rs' = [take numcols $ as ++ repeat nullmixedamt | as <- rs]
                coltotals = foldl' (zipWith (+)) zeros rs'
                grandtotal = sum coltotals
                grandavg | null coltotals = nullmixedamt
                         | otherwise      = grandtotal `divideMixedAmount` fromIntegral (length coltotals)
              in 
                (coltotals, grandtotal, grandavg)
          cbr =
            (title
            ,namedsubreports
            ,overalltotals 
            )
        -- render appropriately
        writeOutput opts $
          case format of
            "csv" -> printCSV (compoundBalanceReportAsCsv ropts cbr) ++ "\n"
            _     -> compoundBalanceReportAsText ropts' cbr

-- | Render a multi-column balance report as plain text suitable for console output.
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

-- | A compound balance report has:
--
-- * an overall title
--
-- * one or more named multi balance reports, with the same column headings
--
-- * a list of overall totals for each column, and their grand total and average
--
-- It is used in compound balance report commands like balancesheet, 
-- cashflow and incomestatement.
type CompoundBalanceReport = 
  ( String
  , [(String, MultiBalanceReport)]
  , ([MixedAmount], MixedAmount, MixedAmount)
  )

-- | Run one subreport for a compound balance command in multi-column mode.
-- This returns a MultiBalanceReport.
compoundBalanceSubreport :: ReportOpts -> Query -> Journal -> (Journal -> Query) -> MultiBalanceReport
compoundBalanceSubreport ropts userq j subreportqfn = r'
  where
    -- force --empty to ensure same columns in all sections
    ropts' = ropts { empty_ = True }
    -- run the report
    q = And [subreportqfn j, userq]
    r@(MultiBalanceReport (dates, rows, totals)) = multiBalanceReport ropts' q j
    -- if user didn't specify --empty, now remove the all-zero rows
    r' | empty_ ropts = r
       | otherwise    = MultiBalanceReport (dates, rows', totals) 
          where
            rows' = filter (not . emptyRow) rows
              where
                emptyRow (_,_,_,amts,_,_) = all isZeroMixedAmount amts

-- | Render a compound balance report as plain text suitable for console output.
{- Eg:
Balance Sheet

             ||  2017/12/31    Total  Average 
=============++===============================
 Assets      ||                               
-------------++-------------------------------
 assets:b    ||           1        1        1 
-------------++-------------------------------
             ||           1        1        1 
=============++===============================
 Liabilities ||                               
-------------++-------------------------------
-------------++-------------------------------
             ||                               
=============++===============================
 Total       ||           1        1        1 

-}
compoundBalanceReportAsText :: ReportOpts -> CompoundBalanceReport -> String
compoundBalanceReportAsText ropts (title, subreports, (coltotals, grandtotal, grandavg)) =
  title ++ "\n\n" ++ 
  renderBalanceReportTable ropts bigtable'
  where
    singlesubreport = length subreports == 1
    bigtable = 
      case map (subreportAsTable ropts singlesubreport) subreports of
        []   -> T.empty
        r:rs -> foldl' concatTables r rs
    bigtable'
      | no_total_ ropts || singlesubreport = 
          bigtable
      | otherwise =
          bigtable
          +====+
          row "Total" (
            coltotals
            ++ (if row_total_ ropts then [grandtotal] else [])
            ++ (if average_ ropts   then [grandavg]   else [])
            )

    -- | Convert a named multi balance report to a table suitable for
    -- concatenating with others to make a compound balance report table.
    subreportAsTable ropts singlesubreport (title, r) = t
      where
        -- unless there's only one section, always show the subtotal row
        ropts' | singlesubreport = ropts
               | otherwise       = ropts{ no_total_=False }
        -- convert to table
        Table lefthdrs tophdrs cells = balanceReportAsTable ropts' r
        -- tweak the layout
        t = Table (T.Group SingleLine [Header title, lefthdrs]) tophdrs ([]:cells)

-- | Render a compound balance report as CSV.
{- Eg: 
ghci> :main -f examples/sample.journal bs -Y -O csv -AT
"Balance Sheet","","","","",""
"Assets","","","","",""
"account","short account","indent","2008","total","average"
"assets:bank:saving","saving","3","$1","$1","$1"
"assets:cash","cash","2","$-2","$-2","$-2"
"totals","","","$-1","$-1","$-1"
"Liabilities","","","","",""
"account","short account","indent","2008","total","average"
"liabilities:debts","debts","2","$1","$1","$1"
"totals","","","$1","$1","$1"
"Total","0","0","0"
-}
compoundBalanceReportAsCsv :: ReportOpts -> CompoundBalanceReport -> CSV
compoundBalanceReportAsCsv ropts (title, subreports, (coltotals, grandtotal, grandavg)) =
  addtotals $
  padRow title :
  concatMap (subreportAsCsv ropts singlesubreport) subreports
  where
    singlesubreport = length subreports == 1
    subreportAsCsv ropts singlesubreport (subreporttitle, multibalreport) =
      padRow subreporttitle :
      multiBalanceReportAsCsv ropts' multibalreport
      where
        -- unless there's only one section, always show the subtotal row
        ropts' | singlesubreport = ropts
               | otherwise       = ropts{ no_total_=False }
    padRow s = take numcols $ s : repeat ""
      where
        numcols
          | null subreports = 1
          | otherwise =
            (3 +) $ -- account name & indent columns
            (if row_total_ ropts then (1+) else id) $
            (if average_ ropts then (1+) else id) $
            maximum $ -- depends on non-null subreports
            map (\(MultiBalanceReport (amtcolheadings, _, _)) -> length amtcolheadings) $ 
            map snd subreports
    addtotals
      | no_total_ ropts || length subreports == 1 = id
      | otherwise = (++ 
          ["Total" :
           map showMixedAmountOneLineWithoutPrice (
             coltotals
             ++ (if row_total_ ropts then [grandtotal] else [])
             ++ (if average_ ropts   then [grandavg]   else [])
             )
          ])