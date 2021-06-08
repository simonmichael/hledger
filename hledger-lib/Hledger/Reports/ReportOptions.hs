{-|

Options common to most hledger reports.

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.Reports.ReportOptions (
  ReportOpts(..),
  ReportSpec(..),
  ReportType(..),
  BalanceType(..),
  AccountListMode(..),
  ValuationType(..),
  defreportopts,
  rawOptsToReportOpts,
  defreportspec,
  reportOptsToSpec,
  updateReportSpec,
  updateReportSpecWith,
  rawOptsToReportSpec,
  balanceTypeOverride,
  flat_,
  tree_,
  reportOptsToggleStatus,
  simplifyStatuses,
  whichDateFromOpts,
  journalApplyValuationFromOpts,
  journalApplyValuationFromOptsWith,
  mixedAmountApplyValuationAfterSumFromOptsWith,
  valuationAfterSum,
  intervalFromRawOpts,
  forecastPeriodFromRawOpts,
  queryFromFlags,
  transactionDateFn,
  postingDateFn,
  reportSpan,
  reportSpanBothDates,
  reportStartDate,
  reportEndDate,
  reportPeriodStart,
  reportPeriodOrJournalStart,
  reportPeriodLastDay,
  reportPeriodOrJournalLastDay,
  reportPeriodName
)
where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.List.Extra (nubSort)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Time.Calendar (Day, addDays)
import Data.Default (Default(..))
import Safe (headMay, lastDef, lastMay, maximumMay)

import Text.Megaparsec.Custom

import Hledger.Data
import Hledger.Query
import Hledger.Utils


-- | What is calculated and shown in each cell in a balance report.
data ReportType = ChangeReport       -- ^ The sum of posting amounts.
                | BudgetReport       -- ^ The sum of posting amounts and the goal.
                | ValueChangeReport  -- ^ The change of value of period-end historical values.
  deriving (Eq, Show)

instance Default ReportType where def = ChangeReport

-- | Which "accumulation method" is being shown in a balance report.
data BalanceType = PeriodChange      -- ^ The accumulate change over a single period.
                 | CumulativeChange  -- ^ The accumulated change across multiple periods.
                 | HistoricalBalance -- ^ The historical ending balance, including the effect of
                                     --   all postings before the report period. Unless altered by,
                                     --   a query, this is what you would see on a bank statement.
  deriving (Eq,Show)

instance Default BalanceType where def = PeriodChange

-- | Should accounts be displayed: in the command's default style, hierarchically, or as a flat list ?
data AccountListMode = ALFlat | ALTree deriving (Eq, Show)

instance Default AccountListMode where def = ALFlat

-- | Standard options for customising report filtering and output.
-- Most of these correspond to standard hledger command-line options
-- or query arguments, but not all. Some are used only by certain
-- commands, as noted below.
data ReportOpts = ReportOpts {
     -- for most reports:
     period_         :: Period
    ,interval_       :: Interval
    ,statuses_       :: [Status]  -- ^ Zero, one, or two statuses to be matched
    ,cost_           :: Costing  -- ^ Should we convert amounts to cost, when present?
    ,value_          :: Maybe ValuationType  -- ^ What value should amounts be converted to ?
    ,infer_value_    :: Bool      -- ^ Infer market prices from transactions ?
    ,depth_          :: Maybe Int
    ,date2_          :: Bool
    ,empty_          :: Bool
    ,no_elide_       :: Bool
    ,real_           :: Bool
    ,format_         :: StringFormat
    ,querystring_    :: [T.Text]
    --
    ,average_        :: Bool
    -- for posting reports (register)
    ,related_        :: Bool
    -- for account transactions reports (aregister)
    ,txn_dates_      :: Bool
    -- for balance reports (bal, bs, cf, is)
    ,reporttype_     :: ReportType
    ,balancetype_    :: BalanceType
    ,accountlistmode_ :: AccountListMode
    ,drop_           :: Int
    ,row_total_      :: Bool
    ,no_total_       :: Bool
    ,show_costs_     :: Bool  -- ^ Whether to show costs for reports which normally don't show them
    ,pretty_tables_  :: Bool
    ,sort_amount_    :: Bool
    ,percent_        :: Bool
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
      -- ^ Whether to use ANSI color codes in text output.
      --   Influenced by the --color/colour flag (cf CliOptions),
      --   whether stdout is an interactive terminal, and the value of
      --   TERM and existence of NO_COLOR environment variables.
    ,forecast_       :: Maybe DateSpan
    ,transpose_      :: Bool
 } deriving (Show)

instance Default ReportOpts where def = defreportopts

defreportopts :: ReportOpts
defreportopts = ReportOpts
    { period_          = PeriodAll
    , interval_        = NoInterval
    , statuses_        = []
    , cost_            = NoCost
    , value_           = Nothing
    , infer_value_     = False
    , depth_           = Nothing
    , date2_           = False
    , empty_           = False
    , no_elide_        = False
    , real_            = False
    , format_          = def
    , querystring_     = []
    , average_         = False
    , related_         = False
    , txn_dates_       = False
    , reporttype_      = def
    , balancetype_     = def
    , accountlistmode_ = ALFlat
    , drop_            = 0
    , row_total_       = False
    , no_total_        = False
    , show_costs_      = False
    , pretty_tables_   = False
    , sort_amount_     = False
    , percent_         = False
    , invert_          = False
    , normalbalance_   = Nothing
    , color_           = False
    , forecast_        = Nothing
    , transpose_       = False
    }

rawOptsToReportOpts :: RawOpts -> IO ReportOpts
rawOptsToReportOpts rawopts = do
    d <- getCurrentDay

    let formatstring = T.pack <$> maybestringopt "format" rawopts
        querystring  = map T.pack $ listofstringopt "args" rawopts  -- doesn't handle an arg like "" right
        (costing, valuation) = valuationTypeFromRawOpts rawopts

    format <- case parseStringFormat <$> formatstring of
        Nothing         -> return defaultBalanceLineFormat
        Just (Right x)  -> return x
        Just (Left err) -> fail $ "could not parse format option: " ++ err

    return defreportopts
          {period_      = periodFromRawOpts d rawopts
          ,interval_    = intervalFromRawOpts rawopts
          ,statuses_    = statusesFromRawOpts rawopts
          ,cost_        = costing
          ,value_       = valuation
          ,infer_value_ = boolopt "infer-market-price" rawopts
          ,depth_       = maybeposintopt "depth" rawopts
          ,date2_       = boolopt "date2" rawopts
          ,empty_       = boolopt "empty" rawopts
          ,no_elide_    = boolopt "no-elide" rawopts
          ,real_        = boolopt "real" rawopts
          ,format_      = format
          ,querystring_ = querystring
          ,average_     = boolopt "average" rawopts
          ,related_     = boolopt "related" rawopts
          ,txn_dates_   = boolopt "txn-dates" rawopts
          ,reporttype_  = reporttypeopt rawopts
          ,balancetype_ = balancetypeopt rawopts
          ,accountlistmode_ = accountlistmodeopt rawopts
          ,drop_        = posintopt "drop" rawopts
          ,row_total_   = boolopt "row-total" rawopts
          ,no_total_    = boolopt "no-total" rawopts
          ,show_costs_  = boolopt "show-costs" rawopts
          ,sort_amount_ = boolopt "sort-amount" rawopts
          ,percent_     = boolopt "percent" rawopts
          ,invert_      = boolopt "invert" rawopts
          ,pretty_tables_ = boolopt "pretty-tables" rawopts
          ,color_       = useColorOnStdout -- a lower-level helper
          ,forecast_    = forecastPeriodFromRawOpts d rawopts
          ,transpose_   = boolopt "transpose" rawopts
          }

-- | The result of successfully parsing a ReportOpts on a particular
-- Day. Any ambiguous dates are completed and Queries are parsed,
-- ensuring that there are no regular expression errors. Values here
-- should be used in preference to re-deriving them from ReportOpts.
-- If you change the query_ in ReportOpts, you should call
-- `reportOptsToSpec` to regenerate the ReportSpec with the new
-- Query.
data ReportSpec = ReportSpec
  { rsOpts      :: ReportOpts  -- ^ The underlying ReportOpts used to generate this ReportSpec
  , rsToday     :: Day         -- ^ The Day this ReportSpec is generated for
  , rsQuery     :: Query       -- ^ The generated Query for the given day
  , rsQueryOpts :: [QueryOpt]  -- ^ A list of QueryOpts for the given day
  } deriving (Show)

instance Default ReportSpec where def = defreportspec

defreportspec :: ReportSpec
defreportspec = ReportSpec
    { rsOpts      = def
    , rsToday     = nulldate
    , rsQuery     = Any
    , rsQueryOpts = []
    }

-- | Generate a ReportSpec from a set of ReportOpts on a given day.
reportOptsToSpec :: Day -> ReportOpts -> Either String ReportSpec
reportOptsToSpec day ropts = do
    (argsquery, queryopts) <- parseQueryList day $ querystring_ ropts
    return ReportSpec
      { rsOpts = ropts
      , rsToday = day
      , rsQuery = simplifyQuery $ And [queryFromFlags ropts, argsquery]
      , rsQueryOpts = queryopts
      }

-- | Update the ReportOpts and the fields derived from it in a ReportSpec,
-- or return an error message if there is a problem such as missing or 
-- unparseable options data. This is the safe way to change a ReportSpec, 
-- ensuring that all fields (rsQuery, rsOpts, querystring_, etc.) are in sync.
updateReportSpec :: ReportOpts -> ReportSpec -> Either String ReportSpec
updateReportSpec ropts rspec = reportOptsToSpec (rsToday rspec) ropts

-- | Like updateReportSpec, but takes a ReportOpts-modifying function.
updateReportSpecWith :: (ReportOpts -> ReportOpts) -> ReportSpec -> Either String ReportSpec
updateReportSpecWith f rspec = reportOptsToSpec (rsToday rspec) . f $ rsOpts rspec

-- | Generate a ReportSpec from RawOpts and the current date.
rawOptsToReportSpec :: RawOpts -> IO ReportSpec
rawOptsToReportSpec rawopts = do
    d <- getCurrentDay
    ropts <- rawOptsToReportOpts rawopts
    either fail return $ reportOptsToSpec d ropts

accountlistmodeopt :: RawOpts -> AccountListMode
accountlistmodeopt =
  fromMaybe ALFlat . choiceopt parse where
    parse = \case
      "tree" -> Just ALTree
      "flat" -> Just ALFlat
      _      -> Nothing

reporttypeopt :: RawOpts -> ReportType
reporttypeopt =
  fromMaybe ChangeReport . choiceopt parse where
    parse = \case
      "sum"         -> Just ChangeReport
      "valuechange" -> Just ValueChangeReport
      "budget"      -> Just BudgetReport
      _             -> Nothing

balancetypeopt :: RawOpts -> BalanceType
balancetypeopt = fromMaybe PeriodChange . balanceTypeOverride

balanceTypeOverride :: RawOpts -> Maybe BalanceType
balanceTypeOverride rawopts = choiceopt parse rawopts <|> reportbal
  where
    parse = \case
      "historical" -> Just HistoricalBalance
      "cumulative" -> Just CumulativeChange
      "change"     -> Just PeriodChange
      _            -> Nothing
    reportbal = case reporttypeopt rawopts of
      ValueChangeReport -> Just PeriodChange
      _                 -> Nothing

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
beginDatesFromRawOpts d = collectopts (begindatefromrawopt d)
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
endDatesFromRawOpts d = collectopts (enddatefromrawopt d)
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
intervalFromRawOpts = lastDef NoInterval . collectopts intervalfromrawopt
  where
    intervalfromrawopt (n,v)
      | n == "period" =
          either
            (\e -> usageError $ "could not parse period option: "++customErrorBundlePretty e)
            extractIntervalOrNothing $
            parsePeriodExpr
              (error' "intervalFromRawOpts: did not expect to need today's date here")  -- PARTIAL: should not happen; we are just getting the interval, which does not use the reference date
              (stripquotes $ T.pack v)
      | n == "daily"     = Just $ Days 1
      | n == "weekly"    = Just $ Weeks 1
      | n == "monthly"   = Just $ Months 1
      | n == "quarterly" = Just $ Quarters 1
      | n == "yearly"    = Just $ Years 1
      | otherwise = Nothing

-- | get period expression from --forecast option
forecastPeriodFromRawOpts :: Day -> RawOpts -> Maybe DateSpan
forecastPeriodFromRawOpts d opts =
  case maybestringopt "forecast" opts
  of
    Nothing -> Nothing
    Just "" -> Just nulldatespan
    Just str ->
      either (\e -> usageError $ "could not parse forecast period : "++customErrorBundlePretty e) (Just . snd) $ 
      parsePeriodExpr d $ stripquotes $ T.pack str

-- | Extract the interval from the parsed -p/--period expression.
-- Return Nothing if an interval is not explicitly defined.
extractIntervalOrNothing :: (Interval, DateSpan) -> Maybe Interval
extractIntervalOrNothing (NoInterval, _) = Nothing
extractIntervalOrNothing (interval, _) = Just interval

-- | Get any statuses to be matched, as specified by -U/--unmarked,
-- -P/--pending, -C/--cleared flags. -UPC is equivalent to no flags,
-- so this returns a list of 0-2 unique statuses.
statusesFromRawOpts :: RawOpts -> [Status]
statusesFromRawOpts = simplifyStatuses . collectopts statusfromrawopt
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
    l' = nubSort l
    numstatuses = length [minBound .. maxBound :: Status]

-- | Add/remove this status from the status list. Used by hledger-ui.
reportOptsToggleStatus s ropts@ReportOpts{statuses_=ss}
  | s `elem` ss = ropts{statuses_=filter (/= s) ss}
  | otherwise   = ropts{statuses_=simplifyStatuses (s:ss)}

-- | Parse the type of valuation and costing to be performed, if any,
-- specified by -B/--cost, -V, -X/--exchange, or --value flags. It is
-- allowed to combine -B/--cost with any other valuation type. If
-- there's more than one valuation type, the rightmost flag wins.
valuationTypeFromRawOpts :: RawOpts -> (Costing, Maybe ValuationType)
valuationTypeFromRawOpts rawopts = (costing, valuation)
  where
    costing   = if (any ((Cost==) . fst) valuationopts) then Cost else NoCost
    valuation = case reporttypeopt rawopts of
        ValueChangeReport -> case directval of
            Nothing        -> Just $ AtEnd Nothing  -- If no valuation requested for valuechange, use AtEnd
            Just (AtEnd _) -> directval             -- If AtEnd valuation requested, use it
            Just _         -> usageError "--valuechange only produces sensible results with --value=end"
        _                  -> directval             -- Otherwise, use requested valuation
      where directval = lastMay $ mapMaybe snd valuationopts

    valuationopts = collectopts valuationfromrawopt rawopts
    valuationfromrawopt (n,v)  -- option name, value
      | n == "B"     = Just (Cost,   Nothing)  -- keep supporting --value=cost for now
      | n == "V"     = Just (NoCost, Just $ AtEnd Nothing)
      | n == "X"     = Just (NoCost, Just $ AtEnd (Just $ T.pack v))
      | n == "value" = Just $ valueopt v
      | otherwise    = Nothing
    valueopt v
      | t `elem` ["cost","c"]  = (Cost,   AtEnd . Just <$> mc)  -- keep supporting --value=cost,COMM for now
      | t `elem` ["then" ,"t"] = (NoCost, Just $ AtThen mc)
      | t `elem` ["end" ,"e"]  = (NoCost, Just $ AtEnd  mc)
      | t `elem` ["now" ,"n"]  = (NoCost, Just $ AtNow  mc)
      | otherwise = case parsedateM t of
            Just d  -> (NoCost, Just $ AtDate d mc)
            Nothing -> usageError $ "could not parse \""++t++"\" as valuation type, should be: then|end|now|t|e|n|YYYY-MM-DD"
      where
        -- parse --value's value: TYPE[,COMM]
        (t,c') = break (==',') v
        mc     = case drop 1 c' of
                   "" -> Nothing
                   c  -> Just $ T.pack c

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
tree_ ReportOpts{accountlistmode_ = ALTree} = True
tree_ ReportOpts{accountlistmode_ = ALFlat} = False

flat_ :: ReportOpts -> Bool
flat_ = not . tree_

-- depthFromOpts :: ReportOpts -> Int
-- depthFromOpts opts = min (fromMaybe 99999 $ depth_ opts) (queryDepth $ queryFromOpts nulldate opts)

-- | Convert this journal's postings' amounts to cost and/or to value, if specified
-- by options (-B/--cost/-V/-X/--value etc.). Strip prices if not needed. This
-- should be the main stop for performing costing and valuation. The exception is
-- whenever you need to perform valuation _after_ summing up amounts, as in a
-- historical balance report with --value=end. valuationAfterSum will check for this
-- condition.
journalApplyValuationFromOpts :: ReportSpec -> Journal -> Journal
journalApplyValuationFromOpts rspec j =
    journalApplyValuationFromOptsWith rspec j priceoracle
  where priceoracle = journalPriceOracle (infer_value_ $ rsOpts rspec) j

-- | Like journalApplyValuationFromOpts, but takes PriceOracle as an argument.
journalApplyValuationFromOptsWith :: ReportSpec -> Journal -> PriceOracle -> Journal
journalApplyValuationFromOptsWith rspec@ReportSpec{rsOpts=ropts} j priceoracle =
    journalMapPostings (valuation . maybeStripPrices) $ costing j
  where
    valuation p = maybe id (postingApplyValuation priceoracle styles (periodEnd p) (rsToday rspec)) (value_ ropts) p
    maybeStripPrices = if show_costs_ ropts then id else postingStripPrices
    costing = case cost_ ropts of
        Cost   -> journalToCost
        NoCost -> id

    -- Find the end of the period containing this posting
    periodEnd  = addDays (-1) . fromMaybe err . mPeriodEnd . postingDate
    mPeriodEnd = spanEnd <=< latestSpanContaining (historical : spans)
    historical = DateSpan Nothing $ spanStart =<< headMay spans
    spans = splitSpan (interval_ ropts) $ reportSpanBothDates j rspec
    styles = journalCommodityStyles j
    err = error "journalApplyValuationFromOpts: expected all spans to have an end date"

-- | Select the Account valuation functions required for performing valuation after summing
-- amounts. Used in MultiBalanceReport to value historical and similar reports.
mixedAmountApplyValuationAfterSumFromOptsWith :: ReportOpts -> Journal -> PriceOracle
                                              -> (DateSpan -> MixedAmount -> MixedAmount)
mixedAmountApplyValuationAfterSumFromOptsWith ropts j priceoracle =
    case valuationAfterSum ropts of
      Just mc -> \span -> valuation mc span . maybeStripPrices . costing
      Nothing -> const id
  where
    valuation mc span = mixedAmountValueAtDate priceoracle styles mc (maybe err (addDays (-1)) $ spanEnd span)
      where err = error "mixedAmountApplyValuationAfterSumFromOptsWith: expected all spans to have an end date"
    maybeStripPrices = if show_costs_ ropts then id else mixedAmountStripPrices
    costing = case cost_ ropts of
        Cost   -> styleMixedAmount styles . mixedAmountCost
        NoCost -> id
    styles = journalCommodityStyles j

-- | If the ReportOpts specify that we are performing valuation after summing amounts,
-- return Just the commodity symbol we're converting to, otherwise return Nothing.
-- Used for example with historical reports with --value=end.
valuationAfterSum :: ReportOpts -> Maybe (Maybe CommoditySymbol)
valuationAfterSum ropts = case value_ ropts of
    Just (AtEnd mc) | valueAfterSum -> Just mc
    _                               -> Nothing
  where valueAfterSum = reporttype_  ropts == ValueChangeReport
                     || balancetype_ ropts /= PeriodChange


-- | Convert report options to a query, ignoring any non-flag command line arguments.
queryFromFlags :: ReportOpts -> Query
queryFromFlags ReportOpts{..} = simplifyQuery $ And flagsq
  where
    flagsq = consIf   Real  real_
           . consJust Depth depth_
           $   [ (if date2_ then Date2 else Date) $ periodAsDateSpan period_
               , Or $ map StatusQ statuses_
               ]
    consIf f b = if b then (f True:) else id
    consJust f = maybe id ((:) . f)

-- Report dates.

-- | The effective report span is the start and end dates specified by
-- options or queries, or otherwise the earliest and latest transaction or
-- posting dates in the journal. If no dates are specified by options/queries
-- and the journal is empty, returns the null date span.
reportSpan :: Journal -> ReportSpec -> DateSpan
reportSpan = reportSpanHelper False

-- | Like reportSpan, but uses both primary and secondary dates when calculating
-- the span.
reportSpanBothDates :: Journal -> ReportSpec -> DateSpan
reportSpanBothDates = reportSpanHelper True

-- | A helper for reportSpan, which takes a Bool indicating whether to use both
-- primary and secondary dates.
reportSpanHelper :: Bool -> Journal -> ReportSpec -> DateSpan
reportSpanHelper bothdates j ReportSpec{rsQuery=query, rsOpts=ropts} = reportspan
  where
    -- The date span specified by -b/-e/-p options and query args if any.
    requestedspan  = dbg3 "requestedspan" $ if bothdates then queryDateSpan' query else queryDateSpan (date2_ ropts) query
    -- If we are requesting period-end valuation, the journal date span should
    -- include price directives after the last transaction
    journalspan = dbg3 "journalspan" $ if bothdates then journalDateSpanBothDates j else journalDateSpan (date2_ ropts) j
    pricespan = dbg3 "pricespan" . DateSpan Nothing $ case value_ ropts of
        Just (AtEnd _) -> fmap (addDays 1) . maximumMay . map pddate $ jpricedirectives j
        _              -> Nothing
    -- If the requested span is open-ended, close it using the journal's start and end dates.
    -- This can still be the null (open) span if the journal is empty.
    requestedspan' = dbg3 "requestedspan'" $ requestedspan `spanDefaultsFrom` (journalspan `spanUnion` pricespan)
    -- The list of interval spans enclosing the requested span.
    -- This list can be empty if the journal was empty,
    -- or if hledger-ui has added its special date:-tomorrow to the query
    -- and all txns are in the future.
    intervalspans  = dbg3 "intervalspans" $ splitSpan (interval_ ropts) requestedspan'
    -- The requested span enlarged to enclose a whole number of intervals.
    -- This can be the null span if there were no intervals.
    reportspan = dbg3 "reportspan" $ DateSpan (spanStart =<< headMay intervalspans)
                                              (spanEnd =<< lastMay intervalspans)

reportStartDate :: Journal -> ReportSpec -> Maybe Day
reportStartDate j = spanStart . reportSpan j

reportEndDate :: Journal -> ReportSpec -> Maybe Day
reportEndDate j = spanEnd . reportSpan j

-- Some pure alternatives to the above. XXX review/clean up

-- Get the report's start date.
-- If no report period is specified, will be Nothing.
reportPeriodStart :: ReportSpec -> Maybe Day
reportPeriodStart = queryStartDate False . rsQuery

-- Get the report's start date, or if no report period is specified,
-- the journal's start date (the earliest posting date). If there's no
-- report period and nothing in the journal, will be Nothing.
reportPeriodOrJournalStart :: ReportSpec -> Journal -> Maybe Day
reportPeriodOrJournalStart rspec j =
  reportPeriodStart rspec <|> journalStartDate False j

-- Get the last day of the overall report period.
-- This the inclusive end date (one day before the
-- more commonly used, exclusive, report end date).
-- If no report period is specified, will be Nothing.
reportPeriodLastDay :: ReportSpec -> Maybe Day
reportPeriodLastDay = fmap (addDays (-1)) . queryEndDate False . rsQuery

-- Get the last day of the overall report period, or if no report
-- period is specified, the last day of the journal (ie the latest
-- posting date). If we're doing period-end valuation, include price
-- directive dates. If there's no report period and nothing in the
-- journal, will be Nothing.
reportPeriodOrJournalLastDay :: ReportSpec -> Journal -> Maybe Day
reportPeriodOrJournalLastDay rspec j = reportPeriodLastDay rspec <|> journalOrPriceEnd
  where
    journalOrPriceEnd = case value_ $ rsOpts rspec of
        Just (AtEnd _) -> max (journalEndDate False j) lastPriceDirective
        _              -> journalEndDate False j
    lastPriceDirective = fmap (addDays 1) . maximumMay . map pddate $ jpricedirectives j

-- | Make a name for the given period in a multiperiod report, given
-- the type of balance being reported and the full set of report
-- periods. This will be used as a column heading (or row heading, in
-- a register summary report). We try to pick a useful name as follows:
--
-- - ending-balance reports: the period's end date
--
-- - balance change reports where the periods are months and all in the same year:
--   the short month name in the current locale
--
-- - all other balance change reports: a description of the datespan,
--   abbreviated to compact form if possible (see showDateSpan).
reportPeriodName :: BalanceType -> [DateSpan] -> DateSpan -> T.Text
reportPeriodName balancetype spans =
  case balancetype of
    PeriodChange -> if multiyear then showDateSpan else showDateSpanMonthAbbrev
      where
        multiyear = (>1) $ length $ nubSort $ map spanStartYear spans
    _ -> maybe "" (showDate . prevday) . spanEnd
