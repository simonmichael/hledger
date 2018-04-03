{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-|

Common helpers for making multi-section balance report commands 
like balancesheet, cashflow, and incomestatement.

-}

module Hledger.Cli.CompoundBalanceCommand (
  CompoundBalanceCommandSpec(..)
 ,CBCSubreportSpec(..)
 ,compoundBalanceCommandMode
 ,compoundBalanceCommand
) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import System.Console.CmdArgs.Explicit as C
import Text.CSV
import Lucid as L
import Text.Tabular as T

import Hledger
import Hledger.Cli.Commands.Balance
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils (writeOutput)

-- | Description of a compound balance report command, 
-- from which we generate the command's cmdargs mode and IO action.
-- A compound balance report command shows one or more sections/subreports, 
-- each with its own title and subtotals row, in a certain order, 
-- plus a grand totals row if there's more than one section.
-- Examples are the balancesheet, cashflow and incomestatement commands.
--
-- Compound balance reports do sign normalisation: they show all account balances 
-- as normally positive, unlike the ordinary BalanceReport and most hledger commands
-- which show income/liability/equity balances as normally negative.  
-- Each subreport specifies the normal sign of its amounts, and whether
-- it should be added to or subtracted from the grand total.
--
data CompoundBalanceCommandSpec = CompoundBalanceCommandSpec {
  cbcname     :: String,              -- ^ command name
  cbcaliases  :: [String],            -- ^ command aliases
  cbchelp     :: String,              -- ^ command line help
  cbctitle    :: String,              -- ^ overall report title
  cbcqueries  :: [CBCSubreportSpec],  -- ^ subreport details
  cbctype     :: BalanceType          -- ^ the "balance" type (change, cumulative, historical) 
                                      --   this report shows (overrides command line flags)
}

-- | Description of one subreport within a compound balance report.
data CBCSubreportSpec = CBCSubreportSpec {
   cbcsubreporttitle :: String
  ,cbcsubreportquery :: Journal -> Query
  ,cbcsubreportnormalsign :: NormalSign
  ,cbcsubreportincreasestotal :: Bool
}

-- | A compound balance report has:
--
-- * an overall title
--
-- * the period (date span) of each column
--
-- * one or more named, normal-positive multi balance reports, 
--   with columns corresponding to the above, and a flag indicating
--   whether they increased or decreased the overall totals
--
-- * a list of overall totals for each column, and their grand total and average
--
-- It is used in compound balance report commands like balancesheet, 
-- cashflow and incomestatement.
type CompoundBalanceReport = 
  ( String
  , [DateSpan]
  , [(String, MultiBalanceReport, Bool)]
  , ([MixedAmount], MixedAmount, MixedAmount)
  )


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
     ,flagNone ["sort-amount","S"] (\opts -> setboolopt "sort-amount" opts) "sort by amount instead of account code/name"
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
compoundBalanceCommand CompoundBalanceCommandSpec{..} opts@CliOpts{reportopts_=ropts, rawopts_=rawopts} j = do
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
      title = cbctitle ++ " " ++ showDateSpan requestedspan ++ maybe "" (' ':) mtitleclarification
        where
          requestedspan = queryDateSpan (date2_ ropts) userq `spanDefaultsFrom` journalDateSpan (date2_ ropts) j
          -- when user overrides, add an indication to the report title
          mtitleclarification = flip fmap mBalanceTypeOverride $ \t ->
            case t of
              PeriodChange      -> "(Balance Changes)"
              CumulativeChange  -> "(Cumulative Ending Balances)"
              HistoricalBalance -> "(Historical Ending Balances)"
      -- Set balance type in the report options.
      -- Also, use tree mode (by default, at least?) if --cumulative/--historical 
      -- are used in single column mode, since in that situation we will be using 
      -- balanceReportFromMultiBalanceReport which does not support eliding boring parents,
      -- and tree mode hides this.. or something.. XXX 
      ropts'
        | not (flat_ ropts) && 
          interval_ ropts==NoInterval && 
          balancetype `elem` [CumulativeChange, HistoricalBalance]
            = ropts{balancetype_=balancetype, accountlistmode_=ALTree}
        | otherwise
            = ropts{balancetype_=balancetype}
      userq = queryFromOpts d ropts'
      format = outputFormatFromOpts opts

      -- make a CompoundBalanceReport
      subreports = 
        map (\CBCSubreportSpec{..} -> 
                (cbcsubreporttitle
                ,mbrNormaliseSign cbcsubreportnormalsign $ -- <- convert normal-negative to normal-positive
                  compoundBalanceSubreport ropts' userq j cbcsubreportquery cbcsubreportnormalsign
                ,cbcsubreportincreasestotal
                ))
            cbcqueries
      subtotalrows = 
        [(coltotals, increasesoveralltotal) 
        | (_, MultiBalanceReport (_,_,(coltotals,_,_)), increasesoveralltotal) <- subreports
        ]
      -- Sum the subreport totals by column. Handle these cases:
      -- - no subreports
      -- - empty subreports, having no subtotals (#588)
      -- - subreports with a shorter subtotals row than the others  
      overalltotals = case subtotalrows of
        [] -> ([], nullmixedamt, nullmixedamt)
        rs ->
          let
            numcols = maximum $ map (length.fst) rs  -- partial maximum is ok, rs is non-null
            paddedsignedsubtotalrows = 
              [map (if increasesoveralltotal then id else negate) $  -- maybe flip the signs
               take numcols $ as ++ repeat nullmixedamt              -- pad short rows with zeros 
              | (as,increasesoveralltotal) <- rs
              ]
            coltotals = foldl' (zipWith (+)) zeros paddedsignedsubtotalrows  -- sum the columns
              where zeros = replicate numcols nullmixedamt
            grandtotal = sum coltotals
            grandavg | null coltotals = nullmixedamt
                     | otherwise      = grandtotal `divideMixedAmount` fromIntegral (length coltotals)
          in 
            (coltotals, grandtotal, grandavg)
      colspans =
        case subreports of
          (_, MultiBalanceReport (ds,_,_), _):_ -> ds
          [] -> []
      cbr =
        (title
        ,colspans
        ,subreports
        ,overalltotals
        )

    -- render appropriately
    writeOutput opts $
      case format of
        "csv"  -> printCSV (compoundBalanceReportAsCsv ropts cbr) ++ "\n"
        "html" -> (++ "\n") $ TL.unpack $ L.renderText $ compoundBalanceReportAsHtml ropts cbr
        _      -> compoundBalanceReportAsText ropts' cbr

-- | Run one subreport for a compound balance command in multi-column mode.
-- This returns a MultiBalanceReport.
compoundBalanceSubreport :: ReportOpts -> Query -> Journal -> (Journal -> Query) -> NormalSign -> MultiBalanceReport
compoundBalanceSubreport ropts userq j subreportqfn subreportnormalsign = r'
  where
    -- force --empty to ensure same columns in all sections
    ropts' = ropts { empty_=True, normalbalance_=Just subreportnormalsign }
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
compoundBalanceReportAsText ropts (title, _colspans, subreports, (coltotals, grandtotal, grandavg)) =
  title ++ "\n\n" ++ 
  balanceReportTableAsText ropts bigtable'
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
          row "Net:" (
            coltotals
            ++ (if row_total_ ropts then [grandtotal] else [])
            ++ (if average_ ropts   then [grandavg]   else [])
            )

    -- | Convert a named multi balance report to a table suitable for
    -- concatenating with others to make a compound balance report table.
    subreportAsTable ropts singlesubreport (title, r, _) = t
      where
        -- unless there's only one section, always show the subtotal row
        ropts' | singlesubreport = ropts
               | otherwise       = ropts{ no_total_=False }
        -- convert to table
        Table lefthdrs tophdrs cells = balanceReportAsTable ropts' r
        -- tweak the layout
        t = Table (T.Group SingleLine [Header title, lefthdrs]) tophdrs ([]:cells)

-- | Add the second table below the first, discarding its column headings.
concatTables (Table hLeft hTop dat) (Table hLeft' _ dat') =
    Table (T.Group DoubleLine [hLeft, hLeft']) hTop (dat ++ dat')

-- | Render a compound balance report as CSV.
-- Subreports' CSV is concatenated, with the headings rows replaced by a
-- subreport title row, and an overall title row, one headings row, and an
-- optional overall totals row is added.
compoundBalanceReportAsCsv :: ReportOpts -> CompoundBalanceReport -> CSV
compoundBalanceReportAsCsv ropts (title, colspans, subreports, (coltotals, grandtotal, grandavg)) =
  addtotals $
  padRow title :
  ("Account" :
   map showDateSpanMonthAbbrev colspans
   ++ (if row_total_ ropts then ["Total"] else [])
   ++ (if average_ ropts then ["Average"] else [])
   ) :
  concatMap (subreportAsCsv ropts singlesubreport) subreports
  where
    singlesubreport = length subreports == 1
    -- | Add a subreport title row and drop the heading row.
    subreportAsCsv ropts singlesubreport (subreporttitle, multibalreport, _) =
      padRow subreporttitle :
      tail (multiBalanceReportAsCsv ropts' multibalreport)
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
            map second3 subreports
    addtotals
      | no_total_ ropts || length subreports == 1 = id
      | otherwise = (++ 
          ["Net:" :
           map showMixedAmountOneLineWithoutPrice (
             coltotals
             ++ (if row_total_ ropts then [grandtotal] else [])
             ++ (if average_ ropts   then [grandavg]   else [])
             )
          ])

-- | Render a compound balance report as HTML.
compoundBalanceReportAsHtml :: ReportOpts -> CompoundBalanceReport -> Html ()
compoundBalanceReportAsHtml ropts cbr =
  let
    (title, colspans, subreports, (coltotals, grandtotal, grandavg)) = cbr
    colspanattr = colspan_ $ TS.pack $ show $ 
      1 + length colspans + (if row_total_ ropts then 1 else 0) + (if average_ ropts then 1 else 0)
    leftattr = style_ "text-align:left"
    blankrow = tr_ $ td_ [colspanattr] $ toHtmlRaw ("&nbsp;"::String)

    titlerows =
         [tr_ $ th_ [colspanattr, leftattr] $ h2_ $ toHtml title]
      ++ [thRow $
          "" :
          map showDateSpanMonthAbbrev colspans
          ++ (if row_total_ ropts then ["Total"] else [])
          ++ (if average_ ropts then ["Average"] else [])
          ]

    thRow :: [String] -> Html ()
    thRow = tr_ . mconcat . map (th_ . toHtml)
    
    -- Make rows for a subreport: its title row, not the headings row,
    -- the data rows, any totals row, and a blank row for whitespace.
    subreportrows :: (String, MultiBalanceReport, Bool) -> [Html ()]
    subreportrows (subreporttitle, mbr, _increasestotal) =
      let
        (_,bodyrows,mtotalsrow) = multiBalanceReportHtmlRows ropts mbr
      in
           [tr_ $ th_ [colspanattr, leftattr] $ toHtml subreporttitle]
        ++ bodyrows
        ++ maybe [] (:[]) mtotalsrow
        ++ [blankrow]

    totalrows | no_total_ ropts || length subreports == 1 = []
              | otherwise =
                  let defstyle = style_ "text-align:right"
                  in
                    [tr_ $ mconcat $
                         th_ [class_ "", style_ "text-align:left"] "Net:"
                       : [th_ [class_ "amount coltotal", defstyle] (toHtml $ showMixedAmountOneLineWithoutPrice a) | a <- coltotals]
                      ++ (if row_total_ ropts then [th_ [class_ "amount coltotal", defstyle] $ toHtml $ showMixedAmountOneLineWithoutPrice $ grandtotal] else [])
                      ++ (if average_ ropts   then [th_ [class_ "amount colaverage", defstyle] $ toHtml $ showMixedAmountOneLineWithoutPrice $ grandavg] else [])
                    ]

  in do
    style_ (TS.unlines [""
      ,"td { padding:0 0.5em; }"
      ,"td:nth-child(1) { white-space:nowrap; }"
      ,"tr:nth-child(even) td { background-color:#eee; }"
      ])
    link_ [rel_ "stylesheet", href_ "hledger.css"]
    table_ $ mconcat $
         titlerows
      ++ [blankrow]
      ++ concatMap subreportrows subreports
      ++ totalrows

