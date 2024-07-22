{-|

Options common to most hledger reports.

-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators #-}

module Hledger.Reports.ReportOptions (
  ReportOpts(..),
  HasReportOptsNoUpdate(..),
  HasReportOpts(..),
  ReportSpec(..),
  HasReportSpec(..),
  SortField(..),
  SortSpec,
  overEither,
  setEither,
  BalanceCalculation(..),
  BalanceAccumulation(..),
  AccountListMode(..),
  ValuationType(..),
  Layout(..),
  defreportopts,
  rawOptsToReportOpts,
  defreportspec,
  defsortspec,
  setDefaultConversionOp,
  reportOptsToSpec,
  updateReportSpec,
  updateReportSpecWith,
  rawOptsToReportSpec,
  balanceAccumulationOverride,
  flat_,
  tree_,
  reportOptsToggleStatus,
  simplifyStatuses,
  whichDate,
  journalValueAndFilterPostings,
  journalValueAndFilterPostingsWith,
  journalApplyValuationFromOpts,
  journalApplyValuationFromOptsWith,
  mixedAmountApplyValuationAfterSumFromOptsWith,
  valuationAfterSum,
  intervalFromRawOpts,
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

import Prelude hiding (Applicative(..))
import Control.Applicative (Applicative(..), Const(..), (<|>))
import Control.Monad ((<=<), guard, join)
import Data.Char (toLower)
import Data.Either (fromRight)
import Data.Either.Extra (eitherToMaybe)
import Data.Functor.Identity (Identity(..))
import Data.List.Extra (find, isPrefixOf, nubSort, stripPrefix)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import Data.Time.Calendar (Day, addDays)
import Data.Default (Default(..))
import Safe (headMay, lastDef, lastMay, maximumMay, readMay)

import Hledger.Data
import Hledger.Query
import Hledger.Utils
import Data.Function ((&))


-- | What to calculate for each cell in a balance report.
-- "Balance report types -> Calculation type" in the hledger manual.
data BalanceCalculation =
    CalcChange        -- ^ Sum of posting amounts in the period.
  | CalcBudget        -- ^ Sum of posting amounts and the goal for the period.
  | CalcValueChange   -- ^ Change from previous period's historical end value to this period's historical end value.
  | CalcGain          -- ^ Change from previous period's gain, i.e. valuation minus cost basis.
  | CalcPostingsCount -- ^ Number of postings in the period.
  deriving (Eq, Show)

instance Default BalanceCalculation where def = CalcChange

-- | How to accumulate calculated values across periods (columns) in a balance report.
-- "Balance report types -> Accumulation type" in the hledger manual.
data BalanceAccumulation =
    PerPeriod   -- ^ No accumulation. Eg, shows the change of balance in each period.
  | Cumulative  -- ^ Accumulate changes across periods, starting from zero at report start.
  | Historical  -- ^ Accumulate changes across periods, including any from before report start.
                --   Eg, shows the historical end balance of each period.
  deriving (Eq,Show)

instance Default BalanceAccumulation where def = PerPeriod

-- | Should accounts be displayed: in the command's default style, hierarchically, or as a flat list ?
data AccountListMode = ALFlat | ALTree deriving (Eq, Show)

instance Default AccountListMode where def = ALFlat

data Layout = LayoutWide (Maybe Int)
            | LayoutTall
            | LayoutBare
            | LayoutTidy
  deriving (Eq, Show)

-- | Standard options for customising report filtering and output.
-- Most of these correspond to standard hledger command-line options
-- or query arguments, but not all. Some are used only by certain
-- commands, as noted below.
data ReportOpts = ReportOpts {
     -- for most reports:
     period_           :: Period
    ,interval_         :: Interval
    ,statuses_         :: [Status]  -- ^ Zero, one, or two statuses to be matched
    ,conversionop_     :: Maybe ConversionOp  -- ^ Which operation should we apply to conversion transactions?
    ,value_            :: Maybe ValuationType  -- ^ What value should amounts be converted to ?
    ,infer_prices_     :: Bool      -- ^ Infer market prices from transactions ?
    ,depth_            :: Maybe Int
    ,date2_            :: Bool
    ,empty_            :: Bool
    ,no_elide_         :: Bool
    ,real_             :: Bool
    ,format_           :: StringFormat
    ,pretty_           :: Bool
    ,querystring_      :: [T.Text]
    --
    ,average_          :: Bool
    -- for posting reports (register)
    ,related_          :: Bool
    -- for sorting reports (register)
    ,sortspec_             :: SortSpec
    -- for account transactions reports (aregister)
    ,txn_dates_        :: Bool
    -- for balance reports (bal, bs, cf, is)
    ,balancecalc_      :: BalanceCalculation  -- ^ What to calculate in balance report cells
    ,balanceaccum_     :: BalanceAccumulation -- ^ How to accumulate balance report values over time
    ,budgetpat_        :: Maybe T.Text  -- ^ A case-insensitive description substring
                                        --   to select periodic transactions for budget reports.
                                        --   (Not a regexp, nor a full hledger query, for now.)
    ,accountlistmode_  :: AccountListMode
    ,drop_             :: Int
    ,declared_         :: Bool  -- ^ Include accounts declared but not yet posted to ?
    ,row_total_        :: Bool
    ,no_total_         :: Bool
    ,summary_only_     :: Bool
    ,show_costs_       :: Bool  -- ^ Show costs for reports which normally don't show them ?
    ,sort_amount_      :: Bool
    ,percent_          :: Bool
    ,invert_           :: Bool  -- ^ Flip all amount signs in reports ?
    ,normalbalance_    :: Maybe NormalSign
      -- ^ This can be set when running balance reports on a set of accounts
      --   with the same normal balance type (eg all assets, or all incomes).
      -- - It helps --sort-amount know how to sort negative numbers
      --   (eg in the income section of an income statement)
      -- - It helps compound balance report commands (is, bs etc.) do
      --   sign normalisation, converting normally negative subreports to
      --   normally positive for a more conventional display.
    ,color_            :: Bool
      -- ^ Whether to use ANSI color codes in text output.
      --   Influenced by the --color/colour flag (cf CliOptions),
      --   whether stdout is an interactive terminal, and the value of
      --   TERM and existence of NO_COLOR environment variables.
    ,transpose_        :: Bool
    ,layout_           :: Layout
 } deriving (Show)

instance Default ReportOpts where def = defreportopts

defreportopts :: ReportOpts
defreportopts = ReportOpts
    { period_           = PeriodAll
    , interval_         = NoInterval
    , statuses_         = []
    , conversionop_     = Nothing
    , value_            = Nothing
    , infer_prices_     = False
    , depth_            = Nothing
    , date2_            = False
    , empty_            = False
    , no_elide_         = False
    , real_             = False
    , format_           = def
    , pretty_           = False
    , querystring_      = []
    , average_          = False
    , related_          = False
    , sortspec_         = defsortspec 
    , txn_dates_        = False
    , balancecalc_      = def
    , balanceaccum_     = def
    , budgetpat_        = Nothing
    , accountlistmode_  = ALFlat
    , drop_             = 0
    , declared_         = False
    , row_total_        = False
    , no_total_         = False
    , summary_only_     = False
    , show_costs_       = False
    , sort_amount_      = False
    , percent_          = False
    , invert_           = False
    , normalbalance_    = Nothing
    , color_            = False
    , transpose_        = False
    , layout_           = LayoutWide Nothing
    }

-- | Generate a ReportOpts from raw command-line input, given a day.
-- This will fail with a usage error if it is passed
-- - an invalid --format argument,
-- - an invalid --value argument,
-- - if --valuechange is called with a valuation type other than -V/--value=end.
-- - an invalid --pretty argument,
rawOptsToReportOpts :: Day -> RawOpts -> ReportOpts
rawOptsToReportOpts d rawopts =

    let formatstring = T.pack <$> maybestringopt "format" rawopts
        querystring  = map T.pack $ listofstringopt "args" rawopts  -- doesn't handle an arg like "" right
        pretty = fromMaybe False $ ynopt "pretty" rawopts

        format = case parseStringFormat <$> formatstring of
            Nothing         -> defaultBalanceLineFormat
            Just (Right x)  -> x
            Just (Left err) -> usageError $ "could not parse format option: " ++ err

    in defreportopts
          {period_           = periodFromRawOpts d rawopts
          ,interval_         = intervalFromRawOpts rawopts
          ,statuses_         = statusesFromRawOpts rawopts
          ,conversionop_     = conversionOpFromRawOpts rawopts
          ,value_            = valuationTypeFromRawOpts rawopts
          ,infer_prices_     = boolopt "infer-market-prices" rawopts
          ,depth_            = maybeposintopt "depth" rawopts
          ,date2_            = boolopt "date2" rawopts
          ,empty_            = boolopt "empty" rawopts
          ,no_elide_         = boolopt "no-elide" rawopts
          ,real_             = boolopt "real" rawopts
          ,format_           = format
          ,querystring_      = querystring
          ,average_          = boolopt "average" rawopts
          ,related_          = boolopt "related" rawopts
          ,sortspec_         = getSortSpec rawopts
          ,txn_dates_        = boolopt "txn-dates" rawopts
          ,balancecalc_      = balancecalcopt rawopts
          ,balanceaccum_     = balanceaccumopt rawopts
          ,budgetpat_        = maybebudgetpatternopt rawopts
          ,accountlistmode_  = accountlistmodeopt rawopts
          ,drop_             = posintopt "drop" rawopts
          ,declared_         = boolopt "declared" rawopts
          ,row_total_        = boolopt "row-total" rawopts
          ,no_total_         = boolopt "no-total" rawopts
          ,summary_only_     = boolopt "summary-only" rawopts
          ,show_costs_       = boolopt "show-costs" rawopts
          ,sort_amount_      = boolopt "sort-amount" rawopts
          ,percent_          = boolopt "percent" rawopts
          ,invert_           = boolopt "invert" rawopts
          ,pretty_           = pretty
          ,color_            = useColorOnStdout -- a lower-level helper
          ,transpose_        = boolopt "transpose" rawopts
          ,layout_           = layoutopt rawopts
          }

-- | A fully-determined set of report parameters 
-- (report options with all partial values made total, eg the begin and end
-- dates are known, avoiding date/regex errors; plus the reporting date),
-- and the query successfully calculated from them.
--
-- If you change the report options or date in one of these, you should
-- use `reportOptsToSpec` to regenerate the whole thing, avoiding inconsistency.
--
data ReportSpec = ReportSpec
  { _rsReportOpts :: ReportOpts  -- ^ The underlying ReportOpts used to generate this ReportSpec
  , _rsDay        :: Day         -- ^ The Day this ReportSpec is generated for
  , _rsQuery      :: Query       -- ^ The generated Query for the given day
  , _rsQueryOpts  :: [QueryOpt]  -- ^ A list of QueryOpts for the given day
  } deriving (Show)

instance Default ReportSpec where def = defreportspec

defreportspec :: ReportSpec
defreportspec = ReportSpec
    { _rsReportOpts = def
    , _rsDay        = nulldate
    , _rsQuery      = Any
    , _rsQueryOpts  = []
    }

-- | Set the default ConversionOp.
setDefaultConversionOp :: ConversionOp -> ReportSpec -> ReportSpec
setDefaultConversionOp defop rspec@ReportSpec{_rsReportOpts=ropts} =
    rspec{_rsReportOpts=ropts{conversionop_=conversionop_ ropts <|> Just defop}}

accountlistmodeopt :: RawOpts -> AccountListMode
accountlistmodeopt =
  fromMaybe ALFlat . choiceopt parse where
    parse = \case
      "tree" -> Just ALTree
      "flat" -> Just ALFlat
      _      -> Nothing

-- Get the argument of the --budget option if any, or the empty string.
maybebudgetpatternopt :: RawOpts -> Maybe T.Text
maybebudgetpatternopt = fmap T.pack . maybestringopt "budget"

balancecalcopt :: RawOpts -> BalanceCalculation
balancecalcopt =
  fromMaybe CalcChange . choiceopt parse where
    parse = \case
      "sum"         -> Just CalcChange
      "valuechange" -> Just CalcValueChange
      "gain"        -> Just CalcGain
      "budget"      -> Just CalcBudget
      "count"       -> Just CalcPostingsCount
      _             -> Nothing

balanceaccumopt :: RawOpts -> BalanceAccumulation
balanceaccumopt = fromMaybe PerPeriod . balanceAccumulationOverride

ynopt :: String -> RawOpts -> Maybe Bool
ynopt opt rawopts = case maybestringopt opt rawopts of
    Just "always" -> Just True
    Just "yes"    -> Just True
    Just "y"      -> Just True
    Just "never"  -> Just False
    Just "no"     -> Just False
    Just "n"      -> Just False
    Just _        -> usageError "--pretty's argument should be \"yes\" or \"no\" (or y, n, always, never)"
    _             -> Nothing

balanceAccumulationOverride :: RawOpts -> Maybe BalanceAccumulation
balanceAccumulationOverride rawopts = choiceopt parse rawopts <|> reportbal
  where
    parse = \case
      "historical" -> Just Historical
      "cumulative" -> Just Cumulative
      "change"     -> Just PerPeriod
      _            -> Nothing
    reportbal = case balancecalcopt rawopts of
      CalcValueChange -> Just PerPeriod
      _               -> Nothing

layoutopt :: RawOpts -> Layout
layoutopt rawopts = fromMaybe (LayoutWide Nothing) $ layout <|> column
  where
    layout = parse <$> maybestringopt "layout" rawopts
    column = LayoutBare <$ guard (boolopt "commodity-column" rawopts)

    parse opt = maybe err snd $ guard (not $ null s) *> find (isPrefixOf s . fst) checkNames
      where
        checkNames = [ ("wide", LayoutWide w)
                     , ("tall", LayoutTall)
                     , ("bare", LayoutBare)
                     , ("tidy", LayoutTidy)
                     ]
        -- For `--layout=elided,n`, elide to the given width
        (s,n) = break (==',') $ map toLower opt
        w = case drop 1 n of
              "" -> Nothing
              c | Just w' <- readMay c -> Just w'
              _ -> usageError "width in --layout=wide,WIDTH must be an integer"

        err = usageError "--layout's argument should be \"wide[,WIDTH]\", \"tall\", \"bare\", or \"tidy\""

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
    (Just b, Just e)   -> simplifyPeriod $ PeriodBetween b e
  where
    mlastb = case beginDatesFromRawOpts d rawopts of
                   [] -> Nothing
                   bs -> Just $ fromEFDay $ last bs
    mlaste = case endDatesFromRawOpts d rawopts of
                   [] -> Nothing
                   es -> Just $ fromEFDay $ last es

-- Get all begin dates specified by -b/--begin or -p/--period options, in order,
-- using the given date to interpret relative date expressions.
beginDatesFromRawOpts :: Day -> RawOpts -> [EFDay]
beginDatesFromRawOpts d = collectopts (begindatefromrawopt d)
  where
    begindatefromrawopt d' (n,v)
      | n == "begin" =
          either (\e -> usageError $ "could not parse "++n++" date: "++customErrorBundlePretty e) Just $
          fixSmartDateStrEither' d' (T.pack v)
      | n == "period" =
        case
          either (\e -> usageError $ "could not parse period option: "++customErrorBundlePretty e) id $
          parsePeriodExpr d' (stripquotes $ T.pack v)
        of
          (_, DateSpan (Just b) _) -> Just b
          _                        -> Nothing
      | otherwise = Nothing

-- Get all end dates specified by -e/--end or -p/--period options, in order,
-- using the given date to interpret relative date expressions.
endDatesFromRawOpts :: Day -> RawOpts -> [EFDay]
endDatesFromRawOpts d = collectopts (enddatefromrawopt d)
  where
    enddatefromrawopt d' (n,v)
      | n == "end" =
          either (\e -> usageError $ "could not parse "++n++" date: "++customErrorBundlePretty e) Just $
          fixSmartDateStrEither' d' (T.pack v)
      | n == "period" =
        case
          either (\e -> usageError $ "could not parse period option: "++customErrorBundlePretty e) id $
          parsePeriodExpr d' (stripquotes $ T.pack v)
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

-- | Parse the type of valuation to be performed, if any, specified by -V,
-- -X/--exchange, or --value flags. If there's more than one valuation type,
-- the rightmost flag wins. This will fail with a usage error if an invalid
-- argument is passed to --value, or if --valuechange is called with a
-- valuation type other than -V/--value=end.
valuationTypeFromRawOpts :: RawOpts -> Maybe ValuationType
valuationTypeFromRawOpts rawopts = case (balancecalcopt rawopts, directval) of
    (CalcValueChange, Nothing       ) -> Just $ AtEnd Nothing  -- If no valuation requested for valuechange, use AtEnd
    (CalcValueChange, Just (AtEnd _)) -> directval             -- If AtEnd valuation requested, use it
    (CalcValueChange, _             ) -> usageError "--valuechange only produces sensible results with --value=end"
    (CalcGain,        Nothing       ) -> Just $ AtEnd Nothing  -- If no valuation requested for gain, use AtEnd
    (_,               _             ) -> directval             -- Otherwise, use requested valuation
  where
    directval = lastMay $ collectopts valuationfromrawopt rawopts
    valuationfromrawopt (n,v)  -- option name, value
      | n == "V"     = Just $ AtEnd Nothing
      | n == "X"     = Just $ AtEnd (Just $ T.pack v)
      | n == "value" = valueopt v
      | otherwise    = Nothing
    valueopt v
      | t `elem` ["cost","c"]  = AtEnd . Just <$> mc  -- keep supporting --value=cost,COMM for now
      | t `elem` ["then" ,"t"] = Just $ AtThen mc
      | t `elem` ["end" ,"e"]  = Just $ AtEnd  mc
      | t `elem` ["now" ,"n"]  = Just $ AtNow  mc
      | otherwise = case parsedateM t of
            Just d  -> Just $ AtDate d mc
            Nothing -> usageError $ "could not parse \""++t++"\" as valuation type, should be: then|end|now|t|e|n|YYYY-MM-DD"
      where
        -- parse --value's value: TYPE[,COMM]
        (t,c') = break (==',') v
        mc     = case drop 1 c' of
                   "" -> Nothing
                   c  -> Just $ T.pack c

-- | Parse the type of costing to be performed, if any, specified by -B/--cost
-- or --value flags. If there's more than one costing type, the rightmost flag
-- wins. This will fail with a usage error if an invalid argument is passed to
-- --cost or if a costing type is requested with --gain.
conversionOpFromRawOpts :: RawOpts -> Maybe ConversionOp
conversionOpFromRawOpts rawopts
    | isJust costFlag && balancecalcopt rawopts == CalcGain = usageError "--gain cannot be combined with --cost"
    | otherwise = costFlag
  where
    costFlag = lastMay $ collectopts conversionopfromrawopt rawopts
    conversionopfromrawopt (n,v)  -- option name, value
      | n == "B"                                    = Just ToCost
      | n == "value", takeWhile (/=',') v `elem` ["cost", "c"] = Just ToCost  -- keep supporting --value=cost for now
      | otherwise                                   = Nothing

-- | Select the Transaction date accessor based on --date2.
transactionDateFn :: ReportOpts -> (Transaction -> Day)
transactionDateFn ReportOpts{..} = if date2_ then transactionDate2 else tdate

-- | Select the Posting date accessor based on --date2.
postingDateFn :: ReportOpts -> (Posting -> Day)
postingDateFn ReportOpts{..} = if date2_ then postingDate2 else postingDate

-- | Report which date we will report on based on --date2.
whichDate :: ReportOpts -> WhichDate
whichDate ReportOpts{..} = if date2_ then SecondaryDate else PrimaryDate

-- | Legacy-compatible convenience aliases for accountlistmode_.
tree_ :: ReportOpts -> Bool
tree_ ReportOpts{accountlistmode_ = ALTree} = True
tree_ ReportOpts{accountlistmode_ = ALFlat} = False

flat_ :: ReportOpts -> Bool
flat_ = not . tree_

-- depthFromOpts :: ReportOpts -> Int
-- depthFromOpts opts = min (fromMaybe 99999 $ depth_ opts) (queryDepth $ queryFromOpts nulldate opts)

-- | Convert a 'Journal''s amounts to cost and/or to value (see
-- 'journalApplyValuationFromOpts'), and filter by the 'ReportSpec' 'Query'.
--
-- We make sure to first filter by amt: and cur: terms, then value the
-- 'Journal', then filter by the remaining terms.
journalValueAndFilterPostings :: ReportSpec -> Journal -> Journal
journalValueAndFilterPostings rspec j = journalValueAndFilterPostingsWith rspec j priceoracle
  where priceoracle = journalPriceOracle (infer_prices_ $ _rsReportOpts rspec) j

-- | Like 'journalValueAndFilterPostings', but takes a 'PriceOracle' as an argument.
journalValueAndFilterPostingsWith :: ReportSpec -> Journal -> PriceOracle -> Journal
journalValueAndFilterPostingsWith rspec@ReportSpec{_rsQuery=q, _rsReportOpts=ropts} j =
    -- Filter by the remainder of the query
      filterJournal reportq
    -- Apply valuation and costing
    . journalApplyValuationFromOptsWith rspec
    -- Filter by amount and currency, so it matches pre-valuation/costing
      (if queryIsNull amtsymq then j else filterJournalAmounts amtsymq j)
  where
    -- with -r, replace each posting with its sibling postings
    filterJournal = if related_ ropts then filterJournalRelatedPostings else filterJournalPostings
    amtsymq = dbg3 "amtsymq" $ filterQuery queryIsAmtOrSym q
    reportq = dbg3 "reportq" $ filterQuery (not . queryIsAmtOrSym) q
    queryIsAmtOrSym = liftA2 (||) queryIsAmt queryIsSym

-- | Convert this journal's postings' amounts to cost and/or to value, if specified
-- by options (-B/--cost/-V/-X/--value etc.). Strip prices if not needed. This
-- should be the main stop for performing costing and valuation. The exception is
-- whenever you need to perform valuation _after_ summing up amounts, as in a
-- historical balance report with --value=end. valuationAfterSum will check for this
-- condition.
journalApplyValuationFromOpts :: ReportSpec -> Journal -> Journal
journalApplyValuationFromOpts rspec j =
  journalApplyValuationFromOptsWith rspec j priceoracle
  where priceoracle = journalPriceOracle (infer_prices_ $ _rsReportOpts rspec) j

-- | Like journalApplyValuationFromOpts, but takes PriceOracle as an argument.
journalApplyValuationFromOptsWith :: ReportSpec -> Journal -> PriceOracle -> Journal
journalApplyValuationFromOptsWith rspec@ReportSpec{_rsReportOpts=ropts} j priceoracle =
  costfn j
  & journalMapPostings (\p -> p
    & dbg9With (lbl "before calc".showMixedAmountOneLine.pamount)
    & postingTransformAmount (calcfn p)
    & dbg9With (lbl (show calc).showMixedAmountOneLine.pamount)
    )
  where
    lbl = lbl_ "journalApplyValuationFromOptsWith"
    -- Which custom calculation to do for balance reports. For all other reports, it will be CalcChange.
    calc = balancecalc_ ropts
    calcfn = case calc of
      CalcGain -> \p -> maybe id (mixedAmountApplyGain      priceoracle styles (postingperiodend p) (_rsDay rspec) (postingDate p)) (value_ ropts)
      _        -> \p -> maybe id (mixedAmountApplyValuation priceoracle styles (postingperiodend p) (_rsDay rspec) (postingDate p)) (value_ ropts)
    costfn = case calc of
      CalcGain -> id
      _        -> journalToCost costop where costop = fromMaybe NoConversionOp $ conversionop_ ropts

    -- Find the end of the period containing this posting
    postingperiodend  = addDays (-1) . fromMaybe err . mPeriodEnd . postingDateOrDate2 (whichDate ropts)
    mPeriodEnd = case interval_ ropts of
        NoInterval -> const . spanEnd . fst $ reportSpan j rspec
        _          -> spanEnd <=< latestSpanContaining (historical : spans)
    historical = DateSpan Nothing $ (fmap Exact . spanStart) =<< headMay spans
    spans = snd $ reportSpanBothDates j rspec
    styles = journalCommodityStyles j
    err = error "journalApplyValuationFromOpts: expected all spans to have an end date"

-- | Select the Account valuation functions required for performing valuation after summing
-- amounts. Used in MultiBalanceReport to value historical and similar reports.
mixedAmountApplyValuationAfterSumFromOptsWith :: ReportOpts -> Journal -> PriceOracle
                                              -> (DateSpan -> MixedAmount -> MixedAmount)
mixedAmountApplyValuationAfterSumFromOptsWith ropts j priceoracle =
    case valuationAfterSum ropts of
        Just mc -> case balancecalc_ ropts of
            CalcGain -> gain mc
            _        -> \spn -> valuation mc spn . costing
        Nothing      -> const id
  where
    valuation mc spn = mixedAmountValueAtDate priceoracle styles mc (maybe err (addDays (-1)) $ spanEnd spn)
    gain mc spn = mixedAmountGainAtDate priceoracle styles mc (maybe err (addDays (-1)) $ spanEnd spn)
    costing = case fromMaybe NoConversionOp $ conversionop_ ropts of
        NoConversionOp -> id
        ToCost         -> styleAmounts styles . mixedAmountCost
    styles = journalCommodityStyles j
    err = error "mixedAmountApplyValuationAfterSumFromOptsWith: expected all spans to have an end date"

-- | If the ReportOpts specify that we are performing valuation after summing amounts,
-- return Just of the commodity symbol we're converting to, Just Nothing for the default,
-- and otherwise return Nothing.
-- Used for example with historical reports with --value=end.
valuationAfterSum :: ReportOpts -> Maybe (Maybe CommoditySymbol)
valuationAfterSum ropts = case value_ ropts of
    Just (AtEnd mc) | valueAfterSum -> Just mc
    _                               -> Nothing
  where valueAfterSum = balancecalc_  ropts == CalcValueChange
                     || balancecalc_  ropts == CalcGain
                     || balanceaccum_ ropts /= PerPeriod


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

-- Methods/types needed for --sort argument

-- Possible arguments taken by the --sort command
-- Each of these takes a bool, which shows if it has been inverted
-- (True -> has been inverted, reverse the order)
data SortField
    = Date' Bool
    | Account' Bool
    | Amount' Bool
    deriving (Show, Eq)
type SortSpec = [SortField]

-- By default, sort by date in ascending order
defsortspec :: SortSpec
defsortspec = [Date' False]

-- Load a SortSpec from the argument given to --sort
-- If there is no spec given, then sort by [Date' False] by default
getSortSpec :: RawOpts -> SortSpec
getSortSpec opts = 
    let opt = maybestringopt "sort" opts
        optParser s = 
          let terms = map strip $ splitAtElement ',' s 
              termParser t = case trimmed of
                "date" -> Date' isNegated
                "account" -> Account' isNegated
                "amount" -> Amount' isNegated
                _ -> error' $ "unsupported field '" ++ t ++ "' given to --sort"
                where isNegated = isPrefixOf "-" t
                      trimmed = fromMaybe t (stripPrefix "-" t)
          in map termParser terms
    in maybe defsortspec optParser opt 


-- Report dates.

-- | The effective report span is the start and end dates specified by
-- options or queries, or otherwise the earliest and latest transaction or
-- posting dates in the journal. If no dates are specified by options/queries
-- and the journal is empty, returns the null date span.
-- Also return the intervals if they are requested.
reportSpan :: Journal -> ReportSpec -> (DateSpan, [DateSpan])
reportSpan = reportSpanHelper False

-- | Like reportSpan, but uses both primary and secondary dates when calculating
-- the span.
reportSpanBothDates :: Journal -> ReportSpec -> (DateSpan, [DateSpan])
reportSpanBothDates = reportSpanHelper True

-- | A helper for reportSpan, which takes a Bool indicating whether to use both
-- primary and secondary dates.
reportSpanHelper :: Bool -> Journal -> ReportSpec -> (DateSpan, [DateSpan])
reportSpanHelper bothdates j ReportSpec{_rsQuery=query, _rsReportOpts=ropts} =
    (reportspan, intervalspans)
  where
    -- The date span specified by -b/-e/-p options and query args if any.
    requestedspan  = dbg3 "requestedspan" $ if bothdates then queryDateSpan' query else queryDateSpan (date2_ ropts) query
    -- If we are requesting period-end valuation, the journal date span should
    -- include price directives after the last transaction
    journalspan = dbg3 "journalspan" $ if bothdates then journalDateSpanBothDates j else journalDateSpan (date2_ ropts) j
    pricespan = dbg3 "pricespan" . DateSpan Nothing $ case value_ ropts of
        Just (AtEnd _) -> fmap (Exact . addDays 1) . maximumMay . map pddate $ jpricedirectives j
        _              -> Nothing
    -- If the requested span is open-ended, close it using the journal's start and end dates.
    -- This can still be the null (open) span if the journal is empty.
    requestedspan' = dbg3 "requestedspan'" $ requestedspan `spanDefaultsFrom` (journalspan `spanExtend` pricespan)
    -- The list of interval spans enclosing the requested span.
    -- This list can be empty if the journal was empty,
    -- or if hledger-ui has added its special date:-tomorrow to the query
    -- and all txns are in the future.
    intervalspans  = dbg3 "intervalspans" $ splitSpan adjust (interval_ ropts) requestedspan'
      where
        -- When calculating report periods, we will adjust the start date back to the nearest interval boundary
        -- unless a start date was specified explicitly.
        adjust = isNothing $ spanStart requestedspan
    -- The requested span enlarged to enclose a whole number of intervals.
    -- This can be the null span if there were no intervals.
    reportspan = dbg3 "reportspan" $ DateSpan (fmap Exact . spanStart =<< headMay intervalspans)
                                              (fmap Exact . spanEnd =<< lastMay intervalspans)

reportStartDate :: Journal -> ReportSpec -> Maybe Day
reportStartDate j = spanStart . fst . reportSpan j

reportEndDate :: Journal -> ReportSpec -> Maybe Day
reportEndDate j = spanEnd . fst . reportSpan j

-- Some pure alternatives to the above. XXX review/clean up

-- Get the report's start date.
-- If no report period is specified, will be Nothing.
reportPeriodStart :: ReportSpec -> Maybe Day
reportPeriodStart = queryStartDate False . _rsQuery

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
reportPeriodLastDay = fmap (addDays (-1)) . queryEndDate False . _rsQuery

-- Get the last day of the overall report period, or if no report
-- period is specified, the last day of the journal (ie the latest
-- posting date). If we're doing period-end valuation, include price
-- directive dates. If there's no report period and nothing in the
-- journal, will be Nothing.
reportPeriodOrJournalLastDay :: ReportSpec -> Journal -> Maybe Day
reportPeriodOrJournalLastDay rspec j = reportPeriodLastDay rspec <|> journalOrPriceEnd
  where
    journalOrPriceEnd = case value_ $ _rsReportOpts rspec of
        Just (AtEnd _) -> max (journalLastDay False j) lastPriceDirective
        _              -> journalLastDay False j
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
reportPeriodName :: BalanceAccumulation -> [DateSpan] -> DateSpan -> T.Text
reportPeriodName balanceaccumulation spans =
  case balanceaccumulation of
    PerPeriod -> if multiyear then showDateSpan else showDateSpanAbbrev
      where
        multiyear = (>1) $ length $ nubSort $ map spanStartYear spans
    _ -> maybe "" (showDate . prevday) . spanEnd

-- lenses

-- Reportable functors are so that we can create special lenses which can fail
-- and report on their failure.
class Functor f => Reportable f e where
    report :: a -> f (Either e a) -> f a

instance Reportable (Const r) e where
    report _ (Const x) = Const x

instance Reportable Identity e where
    report a (Identity i) = Identity $ fromRight a i

instance Reportable Maybe e where
    report _ = (eitherToMaybe =<<)

instance (e ~ a) => Reportable (Either a) e where
    report _ = join

-- | Apply a function over a lens, but report on failure.
overEither :: ((a -> Either e b) -> s -> Either e t) -> (a -> b) -> s -> Either e t
overEither l f = l (pure . f)

-- | Set a field using a lens, but report on failure.
setEither :: ((a -> Either e b) -> s -> Either e t) -> b -> s -> Either e t
setEither l = overEither l . const

type ReportableLens' s a = forall f. Reportable f String => (a -> f a) -> s -> f s

-- | Lenses for ReportOpts.

-- Implement HasReportOptsNoUpdate, the basic lenses for ReportOpts.
makeHledgerClassyLenses ''ReportOpts
makeHledgerClassyLenses ''ReportSpec

-- | Special lenses for ReportOpts which also update the Query and QueryOpts in ReportSpec.
-- Note that these are not true lenses, as they have a further restriction on
-- the functor. This will work as a normal lens for all common uses, but since they
-- don't obey the lens laws for some fancy cases, they may fail in some exotic circumstances.
--
-- Note that setEither/overEither should only be necessary with
-- querystring and reportOpts: the other lenses should never fail.
--
-- === Examples:
-- >>> import Lens.Micro (set)
-- >>> _rsQuery <$> setEither querystring ["assets"] defreportspec
-- Right (Acct (RegexpCI "assets"))
-- >>> _rsQuery <$> setEither querystring ["(assets"] defreportspec
-- Left "This regular expression is invalid or unsupported, please correct it:\n(assets"
-- >>> _rsQuery $ set querystring ["assets"] defreportspec
-- Acct (RegexpCI "assets")
-- >>> _rsQuery $ set querystring ["(assets"] defreportspec
-- *** Exception: Error: Updating ReportSpec failed: try using overEither instead of over or setEither instead of set
-- >>> _rsQuery $ set period (MonthPeriod 2021 08) defreportspec
-- Date DateSpan 2021-08
class HasReportOptsNoUpdate a => HasReportOpts a where
    reportOpts :: ReportableLens' a ReportOpts
    reportOpts = reportOptsNoUpdate
    {-# INLINE reportOpts #-}

    -- XXX these names are a bit clashy

    period :: ReportableLens' a Period
    period = reportOpts.periodNoUpdate
    {-# INLINE period #-}

    statuses :: ReportableLens' a [Status]
    statuses = reportOpts.statusesNoUpdate
    {-# INLINE statuses #-}

    depth :: ReportableLens' a (Maybe Int)
    depth = reportOpts.depthNoUpdate
    {-# INLINE depth #-}

    date2 :: ReportableLens' a Bool
    date2 = reportOpts.date2NoUpdate
    {-# INLINE date2 #-}

    real :: ReportableLens' a Bool
    real = reportOpts.realNoUpdate
    {-# INLINE real #-}

    querystring :: ReportableLens' a [T.Text]
    querystring = reportOpts.querystringNoUpdate
    {-# INLINE querystring #-}

instance HasReportOpts ReportOpts

instance HasReportOptsNoUpdate ReportSpec where
    reportOptsNoUpdate = rsReportOpts

instance HasReportOpts ReportSpec where
    reportOpts f rspec = report (error' "Updating ReportSpec failed: try using overEither instead of over or setEither instead of set") $  -- PARTIAL:
      reportOptsToSpec (_rsDay rspec) <$> f (_rsReportOpts rspec)
    {-# INLINE reportOpts #-}

-- | Generate a ReportSpec from a set of ReportOpts on a given day.
reportOptsToSpec :: Day -> ReportOpts -> Either String ReportSpec
reportOptsToSpec day ropts = do
    (argsquery, queryopts) <- parseQueryList day $ querystring_ ropts
    return ReportSpec
      { _rsReportOpts = ropts
      , _rsDay        = day
      , _rsQuery      = simplifyQuery $ And [queryFromFlags ropts, argsquery]
      , _rsQueryOpts  = queryopts
      }

-- | Update the ReportOpts and the fields derived from it in a ReportSpec,
-- or return an error message if there is a problem such as missing or
-- unparseable options data. This is the safe way to change a ReportSpec,
-- ensuring that all fields (_rsQuery, _rsReportOpts, querystring_, etc.) are in sync.
updateReportSpec :: ReportOpts -> ReportSpec -> Either String ReportSpec
updateReportSpec = setEither reportOpts

-- | Like updateReportSpec, but takes a ReportOpts-modifying function.
updateReportSpecWith :: (ReportOpts -> ReportOpts) -> ReportSpec -> Either String ReportSpec
updateReportSpecWith = overEither reportOpts

-- | Generate a ReportSpec from RawOpts and a provided day, or return an error
-- string if there are regular expression errors.
rawOptsToReportSpec :: Day -> RawOpts -> Either String ReportSpec
rawOptsToReportSpec day = reportOptsToSpec day . rawOptsToReportOpts day
