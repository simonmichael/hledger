{-# LANGUAGE OverloadedStrings, RecordWildCards, QuasiQuotes #-}
{-|

Common helpers for making multi-section balance report commands 
like balancesheet, cashflow, and incomestatement.

-}

module Hledger.Cli.CompoundBalanceCommand (
  compoundBalanceCommandMode
 ,compoundreport
 ,balancesheet
 ,cashflow
 ,incomestatement
 ,compoundreportmode
 ,balancesheetmode
 ,cashflowmode
 ,incomestatementmode
) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Bifunctor (second)
import Data.Char (isSpace)
import Data.Foldable (toList, forM_)
import Data.List (intercalate, foldl', isPrefixOf, find)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Monoid (Sum(..), (<>))
import qualified Data.Set as S
import Data.String.Here
import Data.Text (Text)
import Data.Time.Calendar
import System.Console.CmdArgs.Explicit as C
import System.Exit (exitFailure)
import Text.Tabular as T
import Text.Megaparsec.Pos (sourcePosPretty)

import Hledger
import Hledger.Cli.Balance
import Hledger.Cli.CliOptions

-- | Generate a cmdargs option-parsing mode from a compound balance command 
-- specification.
compoundBalanceCommandMode :: CompoundBalanceCommandSpec -> String -> Mode RawOpts
compoundBalanceCommandMode CompoundBalanceCommandSpec{..} cbchelp = (defCommandMode $ cbcname : toList cbcaliases) {
  modeHelp = cbchelp `withAliases` toList cbcaliases
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
   defType bt | bt == fromMaybe PeriodChange cbctype = " (default)"
              | otherwise                            = ""

-- | Generate a cmdargs option-parsing mode for compound balance commands in
-- general.
compoundreportmode :: Mode RawOpts
compoundreportmode = (defCommandMode ["report", "rep"]) {
  modeHelp = crhelp `withAliases` ["rep"]
 ,modeGroupFlags = C.Group {
     groupUnnamed = [
      flagNone ["change"] (\opts -> setboolopt "change" opts)
        ("show balance change in each period")
     ,flagNone ["cumulative"] (\opts -> setboolopt "cumulative" opts)
        ("show balance change accumulated across periods (in multicolumn reports)")
     ,flagNone ["historical","H"] (\opts -> setboolopt "historical" opts)
        ("show historical ending balance in each period (includes postings before report start date)")
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
   crhelp = [here|
This command displays a formatted compound balance report.  Three reports
are built-in (see balancesheet, incomestatement, and cashflow); however,
these can be modified, and more can be added, by using the 'report'
directive.

The first argument to this command will be the name of the compound balance
report desired.

If no argument is given, a list of all defined compound balance reports are
given.
|]

jcompoundbalance' :: Journal -> M.Map String CompoundBalanceCommandSpec
jcompoundbalance' = M.unionWith fillDefault defaultSpecs
                  . M.fromListWith const
                  . map (\s -> (cbcname s, s))
                  . jcompoundbalance
  where
    -- Use x, but fill in default values from d.  Overwrites the queries
    -- completely of d
    fillDefault d x = x { cbcaliases  = cbcaliases x `S.union` cbcaliases d
                        , cbctitle    = cbctitle x <|> cbctitle d
                        , cbctype     = cbctype  x <|> cbctype  d
                        , cbclocation = cbclocation x <|> cbclocation d
                        }

-- | Generate a runnable command from a compound balance command name
-- provided in the command line
compoundreport :: CliOpts -> Journal -> IO ()
compoundreport o j = case break isSpace . query_ . reportopts_ $ o of
    (cmd, rest) | null cmd  -> displayKnownReports
    -- putStrLn "Report name required" >> exitFailure
                | otherwise ->
      let p c = cmd == cbcname c || cmd `S.member` cbcaliases c
          o'  = modifyQuery (const (dropWhile isSpace rest)) o
      in  case find p (jcompoundbalance' j) of
            Nothing -> putStrLn "No such report defined" >> exitFailure
            Just c  -> compoundBalanceCommand c o' j
  where
    displayKnownReports = do
      putStrLn "Currently defined compound balance reports:"
      forM_ (jcompoundbalance' j) $ \c ->
        let locstr = case cbclocation c of
              Nothing -> "Built-in report"
              Just l  -> "Defined at " ++ sourcePosPretty l
            titlestr = case cbctitle c of
              Nothing -> ""
              Just t  -> " (" ++ t ++ ")"
        in putStrLn $ "  " ++ cbcname c ++ titlestr ++ ": " ++ locstr

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
      balancetype = fromMaybe PeriodChange $ mBalanceTypeOverride <|> cbctype
      -- when user overrides, add an indication to the report title
      title = case (cbctitle, mtitleclarification) of
          (Just t , Just c ) -> Just $ t ++ " " ++ c
          (Nothing, Just c ) -> Just c
          (Just t , Nothing) -> Just t
          (Nothing, Nothing) -> Nothing
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
            foldMap (uncurry (compoundBalanceCommandSingleColumnReport ropts' userq j)) $
              queryStringQueries d cbcqueries
        mapM_ putStrLn title
        putStrLn "\n"
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
            foldMap (uncurry (compoundBalanceCommandMultiColumnReport ropts' userq j)) $
              queryStringQueries d cbcqueries
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
        mapM_ putStrLn title
        putStrLn "\n"
        putStr $ renderBalanceReportTable ropts' overalltable'
  where
    queryStringQueries :: Day -> [(String, Text)] -> [(String, Query)]
    queryStringQueries d = (map . second) (fst . parseQuery d)


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
    -> Query
    -> ([String], Sum MixedAmount)
compoundBalanceCommandSingleColumnReport ropts userq j subreporttitle subreportq = 
  ([subreportstr], Sum total)
  where
    q = And [subreportq, userq]
    r@(_,total)
      -- XXX For --historical/--cumulative, we must use singleBalanceReport;
      -- otherwise we use balanceReport -- because it supports eliding boring parents. 
      -- See also compoundBalanceCommand, Balance.hs -> balance.
      | balancetype_ ropts `elem` [CumulativeChange, HistoricalBalance] = singleBalanceReport ropts q j
      | otherwise                                                       = balanceReport       ropts q j
    subreportstr = intercalate "\n" [subreporttitle <> ":", balanceReportAsText ropts r]

-- | Run one subreport for a compound balance command in multi-column mode.
-- Currently this returns the table of rendered balance amounts, including the
-- totals row; the totals row again, as mixedamounts; and the grand total.
-- The first two are wrapped in a list and the third in a Sum, for easy
-- monoidal combining.
compoundBalanceCommandMultiColumnReport
    :: ReportOpts
    -> Query
    -> Journal
    -> String
    -> Query
    -> ([Table String String MixedAmount], [[MixedAmount]], Sum MixedAmount)
compoundBalanceCommandMultiColumnReport ropts userq j subreporttitle subreportq =
  ([tabl], [coltotals], Sum tot)
  where
    ropts' = ropts { no_total_ = singlesection && no_total_ ropts, empty_ = True }
      where
        singlesection = "Cash" `isPrefixOf` subreporttitle -- TODO temp
    q = And [subreportq, userq]
    -- run the report
    MultiBalanceReport (dates, rows, (coltotals,tot,avg)) = multiBalanceReport ropts' q j
    -- maybe filter all-zero rows from the report
    r = MultiBalanceReport (dates, rows', (coltotals, tot, avg))
      where
        rows' | empty_ ropts = rows
              | otherwise    = filter (not . emptyRow) rows
          where
            emptyRow (_,_,_,amts,_,_) = all isZeroMixedAmount amts
    -- convert to a table for rendering
    Table hLeft hTop dat = balanceReportAsTable ropts' r
    -- tweak the table layout
    tabl = Table (T.Group SingleLine [Header subreporttitle, hLeft]) hTop ([]:dat)

-- * default compound balance reports

balancesheetSpec = CompoundBalanceCommandSpec {
  cbcname     = "balancesheet",
  cbcaliases  = S.singleton "bs",
  cbctitle    = Just "Balance Sheet",
  cbcqueries  = [ ("Assets"     , "^assets?(:|$)"),
                  ("Liabilities", "^(debts?|liabilit(y|ies))(:|$)")
                ],
  cbctype     = Just HistoricalBalance,
  cbclocation = Nothing
}

incomestatementSpec = CompoundBalanceCommandSpec {
  cbcname     = "incomestatement",
  cbcaliases  = S.singleton "is",
  cbctitle    = Just "Income Statement",
  cbcqueries  = [ ("Revenues", "^(income|revenue)s?(:|$)"),
                  ("Expenses", "^expenses?(:|$)")
                ],
  cbctype     = Just PeriodChange,
  cbclocation = Nothing
}

cashflowSpec = CompoundBalanceCommandSpec {
  cbcname     = "cashflow",
  cbcaliases  = S.singleton "cf",
  cbctitle    = Just "Cashflow Statement",
  cbcqueries  = [("Cash flows", "^assets?(:|$) not:(receivable|A/R)")],
  cbctype     = Just PeriodChange,
  cbclocation = Nothing
}

defaultSpecs :: M.Map String CompoundBalanceCommandSpec
defaultSpecs = M.fromList . map (\s -> (cbcname s, s)) $
                 [balancesheetSpec, incomestatementSpec, cashflowSpec]

modifyQuery :: (String -> String) -> CliOpts -> CliOpts
modifyQuery f o@(CliOpts{reportopts_ = ro}) =
    o { reportopts_ = ro { query_ = f (query_ ro) } }

balancesheet :: CliOpts -> Journal -> IO ()
balancesheet = compoundreport . modifyQuery ("balancesheet " ++)

cashflow :: CliOpts -> Journal -> IO ()
cashflow = compoundreport . modifyQuery ("cashflow " ++)

incomestatement :: CliOpts -> Journal -> IO ()
incomestatement = compoundreport . modifyQuery ("incomestatement " ++)

balancesheetmode :: Mode RawOpts
balancesheetmode = compoundBalanceCommandMode balancesheetSpec [here|
This command displays a simple balance sheet, showing historical ending
balances of asset and liability accounts (ignoring any report begin date). 
It assumes that these accounts are under a top-level `asset` or `liability`
account (case insensitive, plural forms also  allowed).
|]

cashflowmode :: Mode RawOpts
cashflowmode = compoundBalanceCommandMode cashflowSpec [here|
This command displays a simple cashflow statement, showing changes
in "cash" accounts. It assumes that these accounts are under a top-level 
`asset` account (case insensitive, plural forms also allowed) and do not 
contain `receivable` or `A/R` in their name. 
|]

incomestatementmode :: Mode RawOpts
incomestatementmode = compoundBalanceCommandMode incomestatementSpec [here|
This command displays a simple income statement, showing revenues
and expenses during a period. It assumes that these accounts are under a 
top-level `revenue` or `income` or `expense` account (case insensitive,
plural forms also allowed).
|]

