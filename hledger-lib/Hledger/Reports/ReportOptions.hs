{-|

Options common to most hledger reports.

-}

{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}

module Hledger.Reports.ReportOptions (
  ReportOpts(..),
  BalanceType(..),
  AccountListMode(..),
  ValuationType(..),
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
  reportPeriodStart,
  reportPeriodOrJournalStart,
  reportPeriodLastDay,
  reportPeriodOrJournalLastDay,
  valuationTypeIsCost,
  valuationTypeIsDefaultValue,

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
     today_          :: Maybe Day  -- ^ The current date. A late addition to ReportOpts.
                                   -- Optional, but when set it may affect some reports:
                                   -- Reports use it when picking a -V valuation date.
                                   -- This is not great, adds indeterminacy.
    ,period_         :: Period
    ,interval_       :: Interval
    ,statuses_       :: [Status]  -- ^ Zero, one, or two statuses to be matched
    ,value_          :: Maybe ValuationType  -- ^ What value should amounts be converted to ?
    ,depth_          :: Maybe Int
    ,display_        :: Maybe DisplayExp  -- XXX unused ?
    ,date2_          :: Bool
    ,empty_          :: Bool
    ,no_elide_       :: Bool
    ,real_           :: Bool
    ,format_         :: Maybe FormatStr
    ,query_          :: String -- ^ All query arguments space sepeareted
                               --   and quoted if needed (see 'quoteIfNeeded')
    --
    ,average_        :: Bool
    -- register command only
    ,related_        :: Bool
    -- balance-type commands only
    ,balancetype_    :: BalanceType
    ,accountlistmode_ :: AccountListMode
    ,drop_           :: Int
    ,row_total_      :: Bool
    ,no_total_       :: Bool
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
     today_       = Just d
    ,period_      = periodFromRawOpts d rawopts'
    ,interval_    = intervalFromRawOpts rawopts'
    ,statuses_    = statusesFromRawOpts rawopts'
    ,value_       = valuationTypeFromRawOpts rawopts'
    ,depth_       = maybeintopt "depth" rawopts'
    ,display_     = maybedisplayopt d rawopts'
    ,date2_       = boolopt "date2" rawopts'
    ,empty_       = boolopt "empty" rawopts'
    ,no_elide_    = boolopt "no-elide" rawopts'
    ,real_        = boolopt "real" rawopts'
    ,format_      = maybestringopt "format" rawopts' -- XXX move to CliOpts or move validation from Cli.CliOptions to here
    ,query_       = unwords . map quoteIfNeeded $ listofstringopt "args" rawopts' -- doesn't handle an arg like "" right
    ,average_     = boolopt "average" rawopts'
    ,related_     = boolopt "related" rawopts'
    ,balancetype_ = balancetypeopt rawopts'
    ,accountlistmode_ = accountlistmodeopt rawopts'
    ,drop_        = intopt "drop" rawopts'
    ,row_total_   = boolopt "row-total" rawopts'
    ,no_total_    = boolopt "no-total" rawopts'
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

-- Get the period specified by any -b/--begin, -e/--end and/or -p/--period
-- options appearing in the command line.
-- Its bounds are the rightmost begin date specified by a -b or -p, and
-- the rightmost end date specified by a -e or -p. Cf #1011.
-- Today's date is provided to help interpret any relative dates.
periodFromRawOpts :: Day -> RawOpts -> Period
periodFromRawOpts d rawopts =
  case (mlastb, mlaste) of
    (Nothing, Nothing) -> PeriodAll
    (Just b, Nothing)  -> PeriodFrom b
    (Nothing, Just e)  -> PeriodTo e
    (Just b, Just e)   -> simplifyPeriod $
                          PeriodBetween b e
  where
    mlastb = case beginDatesFromRawOpts d rawopts of
                   [] -> Nothing
                   bs -> Just $ last bs
    mlaste = case endDatesFromRawOpts d rawopts of
                   [] -> Nothing
                   es -> Just $ last es

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
-- An interval from --period counts only if it is explicitly defined.
intervalFromRawOpts :: RawOpts -> Interval
intervalFromRawOpts = lastDef NoInterval . catMaybes . map intervalfromrawopt
  where
    intervalfromrawopt (n,v)
      | n == "period" =
          either
            (\e -> usageError $ "could not parse period option: "++customErrorBundlePretty e)
            extractIntervalOrNothing $
            parsePeriodExpr
              (error' "intervalFromRawOpts: did not expect to need today's date here") -- should not happen; we are just getting the interval, which does not use the reference date
              (stripquotes $ T.pack v)
      | n == "daily"     = Just $ Days 1
      | n == "weekly"    = Just $ Weeks 1
      | n == "monthly"   = Just $ Months 1
      | n == "quarterly" = Just $ Quarters 1
      | n == "yearly"    = Just $ Years 1
      | otherwise = Nothing

-- | Extract the interval from the parsed -p/--period expression.
-- Return Nothing if an interval is not explicitly defined.
extractIntervalOrNothing :: (Interval, DateSpan) -> Maybe Interval
extractIntervalOrNothing (NoInterval, _) = Nothing
extractIntervalOrNothing (interval, _) = Just interval

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

-- | Parse the type of valuation to be performed, if any, specified by
-- -B/--cost, -V, -X/--exchange, or --value flags. If there's more
-- than one of these, the rightmost flag wins.
valuationTypeFromRawOpts :: RawOpts -> Maybe ValuationType
valuationTypeFromRawOpts = lastDef Nothing . filter isJust . map valuationfromrawopt
  where
    valuationfromrawopt (n,v)  -- option name, value
      | n == "B"     = Just $ AtCost Nothing
      | n == "V"     = Just $ AtDefault Nothing
      | n == "X"     = Just $ AtDefault (Just $ T.pack v)
      | n == "value" = Just $ valuation v
      | otherwise    = Nothing
    valuation v
      | t `elem` ["cost","c"] = AtCost mc
      | t `elem` ["end" ,"e"] = AtEnd  mc
      | t `elem` ["now" ,"n"] = AtNow  mc
      | otherwise =
          case parsedateM t of
            Just d  -> AtDate d mc
            Nothing -> usageError $ "could not parse \""++t++"\" as valuation type, should be: cost|end|now|c|e|n|YYYY-MM-DD"
      where
        -- parse --value's value: TYPE[,COMM]
        (t,c') = break (==',') v
        mc     = case drop 1 c' of
                   "" -> Nothing
                   c  -> Just $ T.pack c

valuationTypeIsCost :: ReportOpts -> Bool
valuationTypeIsCost ropts =
  case value_ ropts of
    Just (AtCost _) -> True
    _               -> False

valuationTypeIsDefaultValue :: ReportOpts -> Bool
valuationTypeIsDefaultValue ropts =
  case value_ ropts of
    Just (AtDefault _) -> True
    _                  -> False

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

-- | Convert this journal's postings' amounts to cost using their
-- transaction prices, if specified by options (-B/--value=cost).
-- Maybe soon superseded by newer valuation code.
journalSelectingAmountFromOpts :: ReportOpts -> Journal -> Journal
journalSelectingAmountFromOpts opts =
  case value_ opts of
    Just (AtCost _) -> journalToCost
    _               -> id

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

-- Report dates.

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

-- Some pure alternatives to the above. XXX review/clean up

-- Get the report's start date.
-- If no report period is specified, will be Nothing.
-- Will also be Nothing if ReportOpts does not have today_ set,
-- since we need that to get the report period robustly
-- (unlike reportStartDate, which looks up the date with IO.)
reportPeriodStart :: ReportOpts -> Maybe Day
reportPeriodStart ropts@ReportOpts{..} = do
  t <- today_
  queryStartDate False $ queryFromOpts t ropts

-- Get the report's start date, or if no report period is specified,
-- the journal's start date (the earliest posting date). If there's no
-- report period and nothing in the journal, will be Nothing.
reportPeriodOrJournalStart :: ReportOpts -> Journal -> Maybe Day
reportPeriodOrJournalStart ropts@ReportOpts{..} j =
  reportPeriodStart ropts <|> journalStartDate False j

-- Get the last day of the overall report period.
-- This the inclusive end date (one day before the
-- more commonly used, exclusive, report end date).
-- If no report period is specified, will be Nothing.
-- Will also be Nothing if ReportOpts does not have today_ set,
-- since we need that to get the report period robustly
-- (unlike reportEndDate, which looks up the date with IO.)
reportPeriodLastDay :: ReportOpts -> Maybe Day
reportPeriodLastDay ropts@ReportOpts{..} = do
  t <- today_
  let q = queryFromOpts t ropts
  qend <- queryEndDate False q
  return $ addDays (-1) qend

-- Get the last day of the overall report period, or if no report
-- period is specified, the last day of the journal (ie the latest
-- posting date). If there's no report period and nothing in the
-- journal, will be Nothing.
reportPeriodOrJournalLastDay :: ReportOpts -> Journal -> Maybe Day
reportPeriodOrJournalLastDay ropts@ReportOpts{..} j =
  reportPeriodLastDay ropts <|> journalEndDate False j

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

