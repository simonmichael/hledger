{-|

Options common to most hledger reports.

-}

{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}

module Hledger.Reports.ReportOptions (
  ReportOpts(..),
  BalanceType(..),
  AccountListMode(..),
  FormatStr,
  defreportopts,
  rawOptsToReportOpts,
  checkReportOpts,
  flat_,
  tree_,
  reportOptsToggleStatus,
  simplifyStatuses,
  whichDateFromOpts,
  journalSelectingAmountFromOpts,
  intervalFromRawOpts,
  queryFromOpts,
  queryFromOptsOnly,
  queryOptsFromOpts,
  transactionDateFn,
  postingDateFn,
  reportSpan,
  reportStartDate,
  reportEndDate,
  specifiedStartEndDates,
  specifiedStartDate,
  specifiedEndDate,

  tests_ReportOptions
)
where

import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Time.Calendar
import Data.Default
import Safe
import System.Console.ANSI (hSupportsANSI)
import System.IO (stdout)
import Text.Megaparsec.Custom

import Hledger.Data
import Hledger.Query
import Hledger.Utils


type FormatStr = String

-- | Which "balance" is being shown in a balance report.
data BalanceType = PeriodChange      -- ^ The change of balance in each period.
                 | CumulativeChange  -- ^ The accumulated change across multiple periods.
                 | HistoricalBalance -- ^ The historical ending balance, including the effect of
                                     --   all postings before the report period. Unless altered by,
                                     --   a query, this is what you would see on a bank statement.
  deriving (Eq,Show,Data,Typeable)

instance Default BalanceType where def = PeriodChange

-- | Should accounts be displayed: in the command's default style, hierarchically, or as a flat list ?
data AccountListMode = ALDefault | ALTree | ALFlat deriving (Eq, Show, Data, Typeable)

instance Default AccountListMode where def = ALDefault

-- | Standard options for customising report filtering and output.
-- Most of these correspond to standard hledger command-line options
-- or query arguments, but not all. Some are used only by certain
-- commands, as noted below. 
data ReportOpts = ReportOpts {
     period_         :: Period
    ,interval_       :: Interval
    ,statuses_       :: [Status]  -- ^ Zero, one, or two statuses to be matched
    ,cost_           :: Bool
    ,depth_          :: Maybe Int
    ,display_        :: Maybe DisplayExp
    ,date2_          :: Bool
    ,empty_          :: Bool
    ,no_elide_       :: Bool
    ,real_           :: Bool
    ,format_         :: Maybe FormatStr
    ,query_          :: String -- all arguments, as a string
    -- register command only
    ,average_        :: Bool
    ,related_        :: Bool
    -- balance-type commands only
    ,balancetype_    :: BalanceType
    ,accountlistmode_ :: AccountListMode
    ,drop_           :: Int
    ,row_total_      :: Bool
    ,no_total_       :: Bool
    ,value_          :: Bool
    ,pretty_tables_  :: Bool
    ,sort_amount_    :: Bool
    ,invert_         :: Bool  -- ^ if true, flip all amount signs in reports
    ,normalbalance_  :: Maybe NormalSign
      -- ^ This can be set when running balance reports on a set of accounts
      --   with the same normal balance type (eg all assets, or all incomes).
      -- - It helps --sort-amount know how to sort negative numbers
      --   (eg in the income section of an income statement) 
      -- - It helps compound balance report commands (is, bs etc.) do  
      --   sign normalisation, converting normally negative subreports to 
      --   normally positive for a more conventional display.   
    ,color_          :: Bool
    ,forecast_       :: Bool
    ,transpose_      :: Bool
 } deriving (Show, Data, Typeable)

instance Default ReportOpts where def = defreportopts

defreportopts :: ReportOpts
defreportopts = ReportOpts
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def

rawOptsToReportOpts :: RawOpts -> IO ReportOpts
rawOptsToReportOpts rawopts = checkReportOpts <$> do
  let rawopts' = checkRawOpts rawopts
  d <- getCurrentDay
  color <- hSupportsANSI stdout
  return defreportopts{
     period_      = periodFromRawOpts d rawopts'
    ,interval_    = intervalFromRawOpts rawopts'
    ,statuses_    = statusesFromRawOpts rawopts'
    ,cost_        = boolopt "cost" rawopts'
    ,depth_       = maybeintopt "depth" rawopts'
    ,display_     = maybedisplayopt d rawopts'
    ,date2_       = boolopt "date2" rawopts'
    ,empty_       = boolopt "empty" rawopts'
    ,no_elide_    = boolopt "no-elide" rawopts'
    ,real_        = boolopt "real" rawopts'
    ,format_      = maybestringopt "format" rawopts' -- XXX move to CliOpts or move validation from Cli.CliOptions to here
    ,query_       = unwords $ listofstringopt "args" rawopts' -- doesn't handle an arg like "" right
    ,average_     = boolopt "average" rawopts'
    ,related_     = boolopt "related" rawopts'
    ,balancetype_ = balancetypeopt rawopts'
    ,accountlistmode_ = accountlistmodeopt rawopts'
    ,drop_        = intopt "drop" rawopts'
    ,row_total_   = boolopt "row-total" rawopts'
    ,no_total_    = boolopt "no-total" rawopts'
    ,value_       = boolopt "value" rawopts'
    ,sort_amount_ = boolopt "sort-amount" rawopts'
    ,invert_      = boolopt "invert" rawopts'
    ,pretty_tables_ = boolopt "pretty-tables" rawopts'
    ,color_       = color
    ,forecast_    = boolopt "forecast" rawopts'
    ,transpose_   = boolopt "transpose" rawopts'
    }

-- | Do extra validation of raw option values, raising an error if there's a problem.
checkRawOpts :: RawOpts -> RawOpts
checkRawOpts rawopts
-- our standard behaviour is to accept conflicting options actually,
-- using the last one - more forgiving for overriding command-line aliases
--   | countopts ["change","cumulative","historical"] > 1
--     = usageError "please specify at most one of --change, --cumulative, --historical"
--   | countopts ["flat","tree"] > 1
--     = usageError "please specify at most one of --flat, --tree"
--   | countopts ["daily","weekly","monthly","quarterly","yearly"] > 1
--     = usageError "please specify at most one of --daily, "
  | otherwise = rawopts
--   where
--     countopts = length . filter (`boolopt` rawopts)

-- | Do extra validation of report options, raising an error if there's a problem.
checkReportOpts :: ReportOpts -> ReportOpts
checkReportOpts ropts@ReportOpts{..} =
  either usageError (const ropts) $ do
    case depth_ of
      Just d | d < 0 -> Left "--depth should have a positive number"
      _              -> Right ()

accountlistmodeopt :: RawOpts -> AccountListMode
accountlistmodeopt rawopts =
  case reverse $ filter (`elem` ["tree","flat"]) $ map fst rawopts of
    ("tree":_) -> ALTree
    ("flat":_) -> ALFlat
    _          -> ALDefault

balancetypeopt :: RawOpts -> BalanceType
balancetypeopt rawopts =
  case reverse $ filter (`elem` ["change","cumulative","historical"]) $ map fst rawopts of
    ("historical":_) -> HistoricalBalance
    ("cumulative":_) -> CumulativeChange
    _                -> PeriodChange

-- Get the period specified by the intersection of -b/--begin, -e/--end and/or
-- -p/--period options, using the given date to interpret relative date expressions.
periodFromRawOpts :: Day -> RawOpts -> Period
periodFromRawOpts d rawopts =
  case (mearliestb, mlateste) of
    (Nothing, Nothing) -> PeriodAll
    (Just b, Nothing)  -> PeriodFrom b
    (Nothing, Just e)  -> PeriodTo e
    (Just b, Just e)   -> simplifyPeriod $
                          PeriodBetween b e
  where
    mearliestb = case beginDatesFromRawOpts d rawopts of
                   [] -> Nothing
                   bs -> Just $ minimum bs
    mlateste   = case endDatesFromRawOpts d rawopts of
                   [] -> Nothing
                   es -> Just $ maximum es

-- Get all begin dates specified by -b/--begin or -p/--period options, in order,
-- using the given date to interpret relative date expressions.
beginDatesFromRawOpts :: Day -> RawOpts -> [Day]
beginDatesFromRawOpts d = catMaybes . map (begindatefromrawopt d)
  where
    begindatefromrawopt d (n,v)
      | n == "begin" =
          either (\e -> usageError $ "could not parse "++n++" date: "++customErrorBundlePretty e) Just $
          fixSmartDateStrEither' d (T.pack v)
      | n == "period" =
        case
          either (\e -> usageError $ "could not parse period option: "++customErrorBundlePretty e) id $
          parsePeriodExpr d (stripquotes $ T.pack v)
        of
          (_, DateSpan (Just b) _) -> Just b
          _                        -> Nothing
      | otherwise = Nothing

-- Get all end dates specified by -e/--end or -p/--period options, in order,
-- using the given date to interpret relative date expressions.
endDatesFromRawOpts :: Day -> RawOpts -> [Day]
endDatesFromRawOpts d = catMaybes . map (enddatefromrawopt d)
  where
    enddatefromrawopt d (n,v)
      | n == "end" =
          either (\e -> usageError $ "could not parse "++n++" date: "++customErrorBundlePretty e) Just $
          fixSmartDateStrEither' d (T.pack v)
      | n == "period" =
        case
          either (\e -> usageError $ "could not parse period option: "++customErrorBundlePretty e) id $
          parsePeriodExpr d (stripquotes $ T.pack v)
        of
          (_, DateSpan _ (Just e)) -> Just e
          _                        -> Nothing
      | otherwise = Nothing

-- | Get the report interval, if any, specified by the last of -p/--period,
-- -D/--daily, -W/--weekly, -M/--monthly etc. options.
intervalFromRawOpts :: RawOpts -> Interval
intervalFromRawOpts = lastDef NoInterval . catMaybes . map intervalfromrawopt
  where
    intervalfromrawopt (n,v)
      | n == "period" =
          either (\e -> usageError $ "could not parse period option: "++customErrorBundlePretty e) (Just . fst) $
          parsePeriodExpr nulldate (stripquotes $ T.pack v) -- reference date does not affect the interval
      | n == "daily"     = Just $ Days 1
      | n == "weekly"    = Just $ Weeks 1
      | n == "monthly"   = Just $ Months 1
      | n == "quarterly" = Just $ Quarters 1
      | n == "yearly"    = Just $ Years 1
      | otherwise = Nothing

-- | Get any statuses to be matched, as specified by -U/--unmarked,
-- -P/--pending, -C/--cleared flags. -UPC is equivalent to no flags,
-- so this returns a list of 0-2 unique statuses.
statusesFromRawOpts :: RawOpts -> [Status]
statusesFromRawOpts = simplifyStatuses . catMaybes . map statusfromrawopt
  where
    statusfromrawopt (n,_)
      | n == "unmarked"  = Just Unmarked
      | n == "pending"   = Just Pending
      | n == "cleared"   = Just Cleared
      | otherwise        = Nothing

-- | Reduce a list of statuses to just one of each status,
-- and if all statuses are present return the empty list.
simplifyStatuses l
  | length l' >= numstatuses = []
  | otherwise                = l'
  where
    l' = nub $ sort l 
    numstatuses = length [minBound .. maxBound :: Status]

-- | Add/remove this status from the status list. Used by hledger-ui.
reportOptsToggleStatus s ropts@ReportOpts{statuses_=ss}
  | s `elem` ss = ropts{statuses_=filter (/= s) ss}
  | otherwise   = ropts{statuses_=simplifyStatuses (s:ss)}

type DisplayExp = String

maybedisplayopt :: Day -> RawOpts -> Maybe DisplayExp
maybedisplayopt d rawopts =
    maybe Nothing (Just . regexReplaceBy "\\[.+?\\]" fixbracketeddatestr) $ maybestringopt "display" rawopts
    where
      fixbracketeddatestr "" = ""
      fixbracketeddatestr s = "[" ++ fixSmartDateStr d (T.pack $ init $ tail s) ++ "]"

-- | Select the Transaction date accessor based on --date2.
transactionDateFn :: ReportOpts -> (Transaction -> Day)
transactionDateFn ReportOpts{..} = if date2_ then transactionDate2 else tdate

-- | Select the Posting date accessor based on --date2.
postingDateFn :: ReportOpts -> (Posting -> Day)
postingDateFn ReportOpts{..} = if date2_ then postingDate2 else postingDate

-- | Report which date we will report on based on --date2.
whichDateFromOpts :: ReportOpts -> WhichDate
whichDateFromOpts ReportOpts{..} = if date2_ then SecondaryDate else PrimaryDate

-- | Legacy-compatible convenience aliases for accountlistmode_.
tree_ :: ReportOpts -> Bool
tree_ = (==ALTree) . accountlistmode_

flat_ :: ReportOpts -> Bool
flat_ = (==ALFlat) . accountlistmode_

-- depthFromOpts :: ReportOpts -> Int
-- depthFromOpts opts = min (fromMaybe 99999 $ depth_ opts) (queryDepth $ queryFromOpts nulldate opts)

-- | Convert this journal's postings' amounts to the cost basis amounts if
-- specified by options.
journalSelectingAmountFromOpts :: ReportOpts -> Journal -> Journal
journalSelectingAmountFromOpts opts
    | cost_ opts = journalConvertAmountsToCost
    | otherwise = id

-- | Convert report options and arguments to a query.
queryFromOpts :: Day -> ReportOpts -> Query
queryFromOpts d ReportOpts{..} = simplifyQuery $ And $ [flagsq, argsq]
  where
    flagsq = And $
              [(if date2_ then Date2 else Date) $ periodAsDateSpan period_]
              ++ (if real_ then [Real True] else [])
              ++ (if empty_ then [Empty True] else []) -- ?
              ++ [Or $ map StatusQ statuses_]
              ++ (maybe [] ((:[]) . Depth) depth_)
    argsq = fst $ parseQuery d (T.pack query_)

-- | Convert report options to a query, ignoring any non-flag command line arguments.
queryFromOptsOnly :: Day -> ReportOpts -> Query
queryFromOptsOnly _d ReportOpts{..} = simplifyQuery flagsq
  where
    flagsq = And $
              [(if date2_ then Date2 else Date) $ periodAsDateSpan period_]
              ++ (if real_ then [Real True] else [])
              ++ (if empty_ then [Empty True] else []) -- ?
              ++ [Or $ map StatusQ statuses_]
              ++ (maybe [] ((:[]) . Depth) depth_)

-- | Convert report options and arguments to query options.
queryOptsFromOpts :: Day -> ReportOpts -> [QueryOpt]
queryOptsFromOpts d ReportOpts{..} = flagsqopts ++ argsqopts
  where
    flagsqopts = []
    argsqopts = snd $ parseQuery d (T.pack query_)

-- | The effective report span is the start and end dates specified by
-- options or queries, or otherwise the earliest and latest transaction or 
-- posting dates in the journal. If no dates are specified by options/queries
-- and the journal is empty, returns the null date span.
-- Needs IO to parse smart dates in options/queries.
reportSpan :: Journal -> ReportOpts -> IO DateSpan
reportSpan j ropts = do
  (mspecifiedstartdate, mspecifiedenddate) <-
    dbg2 "specifieddates" <$> specifiedStartEndDates ropts
  let
    DateSpan mjournalstartdate mjournalenddate =
      dbg2 "journalspan" $ journalDateSpan False j  -- ignore secondary dates
    mstartdate = mspecifiedstartdate <|> mjournalstartdate
    menddate   = mspecifiedenddate   <|> mjournalenddate
  return $ dbg1 "reportspan" $ DateSpan mstartdate menddate

reportStartDate :: Journal -> ReportOpts -> IO (Maybe Day)
reportStartDate j ropts = spanStart <$> reportSpan j ropts

reportEndDate :: Journal -> ReportOpts -> IO (Maybe Day)
reportEndDate j ropts = spanEnd <$> reportSpan j ropts

-- | The specified report start/end dates are the dates specified by options or queries, if any.
-- Needs IO to parse smart dates in options/queries.
specifiedStartEndDates :: ReportOpts -> IO (Maybe Day, Maybe Day)
specifiedStartEndDates ropts = do
  today <- getCurrentDay
  let
    q = queryFromOpts today ropts
    mspecifiedstartdate = queryStartDate False q
    mspecifiedenddate   = queryEndDate   False q
  return (mspecifiedstartdate, mspecifiedenddate)

specifiedStartDate :: ReportOpts -> IO (Maybe Day)
specifiedStartDate ropts = fst <$> specifiedStartEndDates ropts

specifiedEndDate :: ReportOpts -> IO (Maybe Day)
specifiedEndDate ropts = snd <$> specifiedStartEndDates ropts

-- tests

tests_ReportOptions = tests "ReportOptions" [
   tests "queryFromOpts" [
      (queryFromOpts nulldate defreportopts) `is` Any
     ,(queryFromOpts nulldate defreportopts{query_="a"}) `is` (Acct "a")
     ,(queryFromOpts nulldate defreportopts{query_="desc:'a a'"}) `is` (Desc "a a")
     ,(queryFromOpts nulldate defreportopts{period_=PeriodFrom (parsedate "2012/01/01"),query_="date:'to 2013'" }) 
      `is` (Date $ mkdatespan "2012/01/01" "2013/01/01")
     ,(queryFromOpts nulldate defreportopts{query_="date2:'in 2012'"}) `is` (Date2 $ mkdatespan "2012/01/01" "2013/01/01")
     ,(queryFromOpts nulldate defreportopts{query_="'a a' 'b"}) `is` (Or [Acct "a a", Acct "'b"])
     ]

  ,tests "queryOptsFromOpts" [
      (queryOptsFromOpts nulldate defreportopts) `is` []
     ,(queryOptsFromOpts nulldate defreportopts{query_="a"}) `is` []
     ,(queryOptsFromOpts nulldate defreportopts{period_=PeriodFrom (parsedate "2012/01/01")
                                                                   ,query_="date:'to 2013'"
                                                                   })
      `is` []
    ]
 ]

