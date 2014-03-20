{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
{-|

Reusable report-related options.

-}

module Hledger.Reports.ReportOptions (
  ReportOpts(..),
  BalanceType(..),
  DisplayExp,
  FormatStr,
  defreportopts,
  dateSpanFromOpts,
  intervalFromOpts,
  clearedValueFromOpts,
  whichDateFromOpts,
  journalSelectingAmountFromOpts,
  queryFromOpts,
  queryFromOptsOnly,
  queryOptsFromOpts,
  reportSpans,
  transactionDateFn,
  postingDateFn,

  -- * Tests
  tests_Hledger_Reports_ReportOptions
)
where

import Data.Time.Calendar
import Safe (headMay, lastMay)
import System.Console.CmdArgs  -- for defaults support
import Test.HUnit

import Hledger.Data
import Hledger.Query
import Hledger.Utils


-- | Standard options for customising report filtering and output,
-- corresponding to hledger's command-line options and query language
-- arguments. Used in hledger-lib and above.
data ReportOpts = ReportOpts {
     begin_          :: Maybe Day
    ,end_            :: Maybe Day
    ,period_         :: Maybe (Interval,DateSpan)
    ,cleared_        :: Bool
    ,uncleared_      :: Bool
    ,cost_           :: Bool
    ,depth_          :: Maybe Int
    ,display_        :: Maybe DisplayExp
    ,date2_          :: Bool
    ,empty_          :: Bool
    ,no_elide_       :: Bool
    ,real_           :: Bool
    ,balancetype_    :: BalanceType -- for balance command
    ,flat_           :: Bool -- for balance command
    ,drop_           :: Int  -- "
    ,no_total_       :: Bool -- "
    ,daily_          :: Bool
    ,weekly_         :: Bool
    ,monthly_        :: Bool
    ,quarterly_      :: Bool
    ,yearly_         :: Bool
    ,format_         :: Maybe FormatStr
    ,related_        :: Bool
    ,average_        :: Bool
    ,query_          :: String -- all arguments, as a string
 } deriving (Show, Data, Typeable)

type DisplayExp = String
type FormatStr = String

-- | Which balance is being shown in a multi-column balance report.
data BalanceType = PeriodBalance     -- ^ The change of balance in each period.
                 | CumulativeBalance -- ^ The accumulated balance at each period's end, starting from zero at the report start date.
                 | HistoricalBalance -- ^ The historical balance at each period's end, starting from the account balances at the report start date.
  deriving (Eq,Show,Data,Typeable)
instance Default BalanceType where def = PeriodBalance

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

instance Default ReportOpts where def = defreportopts

-- | Figure out the date span we should report on, based on any
-- begin/end/period options provided. A period option will cause begin and
-- end options to be ignored.
dateSpanFromOpts :: Day -> ReportOpts -> DateSpan
dateSpanFromOpts _ ReportOpts{..} =
    case period_ of Just (_,span) -> span
                    Nothing -> DateSpan begin_ end_

-- | Figure out the reporting interval, if any, specified by the options.
-- --period overrides --daily overrides --weekly overrides --monthly etc.
intervalFromOpts :: ReportOpts -> Interval
intervalFromOpts ReportOpts{..} =
    case period_ of
      Just (interval,_) -> interval
      Nothing -> i
          where i | daily_ = Days 1
                  | weekly_ = Weeks 1
                  | monthly_ = Months 1
                  | quarterly_ = Quarters 1
                  | yearly_ = Years 1
                  | otherwise =  NoInterval

-- | Get a maybe boolean representing the last cleared/uncleared option if any.
clearedValueFromOpts :: ReportOpts -> Maybe Bool
clearedValueFromOpts ReportOpts{..} | cleared_   = Just True
                                    | uncleared_ = Just False
                                    | otherwise  = Nothing

-- depthFromOpts :: ReportOpts -> Int
-- depthFromOpts opts = min (fromMaybe 99999 $ depth_ opts) (queryDepth $ queryFromOpts nulldate opts)

-- | Report which date we will report on based on --date2.
whichDateFromOpts :: ReportOpts -> WhichDate
whichDateFromOpts ReportOpts{..} = if date2_ then SecondaryDate else PrimaryDate

-- | Select the Transaction date accessor based on --date2.
transactionDateFn :: ReportOpts -> (Transaction -> Day)
transactionDateFn ReportOpts{..} = if date2_ then transactionDate2 else tdate

-- | Select the Posting date accessor based on --date2.
postingDateFn :: ReportOpts -> (Posting -> Day)
postingDateFn ReportOpts{..} = if date2_ then postingDate2 else postingDate


-- | Convert this journal's postings' amounts to the cost basis amounts if
-- specified by options.
journalSelectingAmountFromOpts :: ReportOpts -> Journal -> Journal
journalSelectingAmountFromOpts opts
    | cost_ opts = journalConvertAmountsToCost
    | otherwise = id

-- | Convert report options and arguments to a query.
queryFromOpts :: Day -> ReportOpts -> Query
queryFromOpts d opts@ReportOpts{..} = simplifyQuery $ And $ [flagsq, argsq]
  where
    flagsq = And $
              [(if date2_ then Date2 else Date) $ dateSpanFromOpts d opts]
              ++ (if real_ then [Real True] else [])
              ++ (if empty_ then [Empty True] else []) -- ?
              ++ (maybe [] ((:[]) . Status) (clearedValueFromOpts opts))
              ++ (maybe [] ((:[]) . Depth) depth_)
    argsq = fst $ parseQuery d query_

-- | Convert report options to a query, ignoring any non-flag command line arguments.
queryFromOptsOnly :: Day -> ReportOpts -> Query
queryFromOptsOnly d opts@ReportOpts{..} = simplifyQuery flagsq
  where
    flagsq = And $
              [(if date2_ then Date2 else Date) $ dateSpanFromOpts d opts]
              ++ (if real_ then [Real True] else [])
              ++ (if empty_ then [Empty True] else []) -- ?
              ++ (maybe [] ((:[]) . Status) (clearedValueFromOpts opts))
              ++ (maybe [] ((:[]) . Depth) depth_)

tests_queryFromOpts = [
 "queryFromOpts" ~: do
  assertEqual "" Any (queryFromOpts nulldate defreportopts)
  assertEqual "" (Acct "a") (queryFromOpts nulldate defreportopts{query_="a"})
  assertEqual "" (Desc "a a") (queryFromOpts nulldate defreportopts{query_="desc:'a a'"})
  assertEqual "" (Date $ mkdatespan "2012/01/01" "2013/01/01")
                 (queryFromOpts nulldate defreportopts{begin_=Just (parsedate "2012/01/01")
                                                      ,query_="date:'to 2013'"
                                                      })
  assertEqual "" (Date2 $ mkdatespan "2012/01/01" "2013/01/01")
                 (queryFromOpts nulldate defreportopts{query_="edate:'in 2012'"})
  assertEqual "" (Or [Acct "a a", Acct "'b"])
                 (queryFromOpts nulldate defreportopts{query_="'a a' 'b"})
 ]

-- | Convert report options and arguments to query options.
queryOptsFromOpts :: Day -> ReportOpts -> [QueryOpt]
queryOptsFromOpts d ReportOpts{..} = flagsqopts ++ argsqopts
  where
    flagsqopts = []
    argsqopts = snd $ parseQuery d query_

tests_queryOptsFromOpts = [
 "queryOptsFromOpts" ~: do
  assertEqual "" [] (queryOptsFromOpts nulldate defreportopts)
  assertEqual "" [] (queryOptsFromOpts nulldate defreportopts{query_="a"})
  assertEqual "" [] (queryOptsFromOpts nulldate defreportopts{begin_=Just (parsedate "2012/01/01")
                                                             ,query_="date:'to 2013'"
                                                             })
 ]

-- | Calculate the overall span and per-period date spans for a report
-- based on command-line options, the parsed search query, and the
-- journal data. If a reporting interval is specified, the report span
-- will be enlarged to include a whole number of report periods.
-- Reports will sometimes trim these spans further when appropriate.
reportSpans ::  ReportOpts -> Query -> Journal -> (DateSpan, [DateSpan])
reportSpans opts q j = (reportspan, spans)
  where
    -- get the requested span from the query, which is based on
    -- -b/-e/-p opts and query args.
    requestedspan = queryDateSpan (date2_ opts) q

    -- set the start and end date to the journal's if not specified
    requestedspan' = requestedspan `orDatesFrom` journalDateSpan j

    -- if there's a reporting interval, calculate the report periods
    -- which enclose the requested span
    spans = dbg "spans" $ splitSpan (intervalFromOpts opts) requestedspan'

    -- the overall report span encloses the periods
    reportspan = DateSpan
                 (maybe Nothing spanStart $ headMay spans)
                 (maybe Nothing spanEnd   $ lastMay spans)

tests_Hledger_Reports_ReportOptions :: Test
tests_Hledger_Reports_ReportOptions = TestList $
    tests_queryFromOpts
 ++ tests_queryOptsFromOpts
