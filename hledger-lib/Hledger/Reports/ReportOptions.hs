{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
{-|

Options common to most hledger reports.

-}

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
  dateSpanFromOpts,
  intervalFromOpts,
  clearedValueFromOpts,
  whichDateFromOpts,
  journalSelectingAmountFromOpts,
  queryFromOpts,
  queryFromOptsOnly,
  queryOptsFromOpts,
  transactionDateFn,
  postingDateFn,

  tests_Hledger_Reports_ReportOptions
)
where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Time.Calendar
import System.Console.CmdArgs.Default  -- some additional default stuff
import Test.HUnit

import Hledger.Data
import Hledger.Query
import Hledger.Utils


type FormatStr = String

-- | Which balance is being shown in a multi-column balance report.
data BalanceType = PeriodBalance     -- ^ The change of balance in each period.
                 | CumulativeBalance -- ^ The accumulated balance at each period's end, starting from zero at the report start date.
                 | HistoricalBalance -- ^ The historical balance at each period's end, starting from the account balances at the report start date.
  deriving (Eq,Show,Data,Typeable)

instance Default BalanceType where def = PeriodBalance

-- | Should accounts be displayed: in the command's default style, hierarchically, or as a flat list ?
data AccountListMode = ALDefault | ALTree | ALFlat deriving (Eq, Show, Data, Typeable)

instance Default AccountListMode where def = ALDefault

-- | Standard options for customising report filtering and output,
-- corresponding to hledger's command-line options and query language
-- arguments. Used in hledger-lib and above.
data ReportOpts = ReportOpts {
     begin_          :: Maybe Day
    ,end_            :: Maybe Day
    ,period_         :: Maybe (Interval,DateSpan)
    ,cleared_        :: Bool
    ,pending_        :: Bool
    ,uncleared_      :: Bool
    ,cost_           :: Bool
    ,depth_          :: Maybe Int
    ,display_        :: Maybe DisplayExp
    ,date2_          :: Bool
    ,empty_          :: Bool
    ,no_elide_       :: Bool
    ,real_           :: Bool
    ,daily_          :: Bool
    ,weekly_         :: Bool
    ,monthly_        :: Bool
    ,quarterly_      :: Bool
    ,yearly_         :: Bool
    ,format_         :: Maybe FormatStr
    ,query_          :: String -- all arguments, as a string
    -- register
    ,average_        :: Bool
    ,related_        :: Bool
    -- balance
    ,balancetype_    :: BalanceType
    ,accountlistmode_  :: AccountListMode
    ,drop_           :: Int
    ,row_total_      :: Bool
    ,no_total_       :: Bool
    ,value_          :: Bool
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
    def

rawOptsToReportOpts :: RawOpts -> IO ReportOpts
rawOptsToReportOpts rawopts = checkReportOpts <$> do
  d <- getCurrentDay
  return defreportopts{
     begin_       = maybesmartdateopt d "begin" rawopts
    ,end_         = maybesmartdateopt d "end" rawopts
    ,period_      = maybeperiodopt d rawopts
    ,cleared_     = boolopt "cleared" rawopts
    ,pending_     = boolopt "pending" rawopts
    ,uncleared_   = boolopt "uncleared" rawopts
    ,cost_        = boolopt "cost" rawopts
    ,depth_       = maybeintopt "depth" rawopts
    ,display_     = maybedisplayopt d rawopts
    ,date2_       = boolopt "date2" rawopts
    ,empty_       = boolopt "empty" rawopts
    ,no_elide_    = boolopt "no-elide" rawopts
    ,real_        = boolopt "real" rawopts
    ,daily_       = boolopt "daily" rawopts
    ,weekly_      = boolopt "weekly" rawopts
    ,monthly_     = boolopt "monthly" rawopts
    ,quarterly_   = boolopt "quarterly" rawopts
    ,yearly_      = boolopt "yearly" rawopts
    ,format_      = maybestringopt "format" rawopts -- XXX move to CliOpts or move validation from Cli.Options to here
    ,query_       = unwords $ listofstringopt "args" rawopts -- doesn't handle an arg like "" right
    ,average_     = boolopt "average" rawopts
    ,related_     = boolopt "related" rawopts
    ,balancetype_ = balancetypeopt rawopts
    ,accountlistmode_ = accountlistmodeopt rawopts
    ,drop_        = intopt "drop" rawopts
    ,row_total_   = boolopt "row-total" rawopts
    ,no_total_    = boolopt "no-total" rawopts
    ,value_       = boolopt "value" rawopts
    }

-- | Do extra validation of opts, raising an error if there is trouble.
checkReportOpts :: ReportOpts -> ReportOpts
checkReportOpts ropts@ReportOpts{..} =
  either optserror (const ropts) $ do
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
balancetypeopt rawopts
    | length [o | o <- ["cumulative","historical"], isset o] > 1
                         = optserror "please specify at most one of --cumulative and --historical"
    | isset "cumulative" = CumulativeBalance
    | isset "historical" = HistoricalBalance
    | otherwise          = PeriodBalance
    where
      isset = flip boolopt rawopts

maybesmartdateopt :: Day -> String -> RawOpts -> Maybe Day
maybesmartdateopt d name rawopts =
        case maybestringopt name rawopts of
          Nothing -> Nothing
          Just s -> either
                    (\e -> optserror $ "could not parse "++name++" date: "++show e)
                    Just
                    $ fixSmartDateStrEither' d s

type DisplayExp = String

maybedisplayopt :: Day -> RawOpts -> Maybe DisplayExp
maybedisplayopt d rawopts =
    maybe Nothing (Just . regexReplaceBy "\\[.+?\\]" fixbracketeddatestr) $ maybestringopt "display" rawopts
    where
      fixbracketeddatestr "" = ""
      fixbracketeddatestr s = "[" ++ fixSmartDateStr d (init $ tail s) ++ "]"

maybeperiodopt :: Day -> RawOpts -> Maybe (Interval,DateSpan)
maybeperiodopt d rawopts =
    case maybestringopt "period" rawopts of
      Nothing -> Nothing
      Just s -> either
                (\e -> optserror $ "could not parse period option: "++show e)
                Just
                $ parsePeriodExpr d s

-- | Legacy-compatible convenience aliases for accountlistmode_.
tree_ :: ReportOpts -> Bool
tree_ = (==ALTree) . accountlistmode_

flat_ :: ReportOpts -> Bool
flat_ = (==ALFlat) . accountlistmode_

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
clearedValueFromOpts :: ReportOpts -> Maybe ClearedStatus
clearedValueFromOpts ReportOpts{..} | cleared_   = Just Cleared
                                    | pending_   = Just Pending
                                    | uncleared_ = Just Uncleared
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

tests_queryFromOpts :: [Test]
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
                 (queryFromOpts nulldate defreportopts{query_="date2:'in 2012'"})
  assertEqual "" (Or [Acct "a a", Acct "'b"])
                 (queryFromOpts nulldate defreportopts{query_="'a a' 'b"})
 ]

-- | Convert report options and arguments to query options.
queryOptsFromOpts :: Day -> ReportOpts -> [QueryOpt]
queryOptsFromOpts d ReportOpts{..} = flagsqopts ++ argsqopts
  where
    flagsqopts = []
    argsqopts = snd $ parseQuery d query_

tests_queryOptsFromOpts :: [Test]
tests_queryOptsFromOpts = [
 "queryOptsFromOpts" ~: do
  assertEqual "" [] (queryOptsFromOpts nulldate defreportopts)
  assertEqual "" [] (queryOptsFromOpts nulldate defreportopts{query_="a"})
  assertEqual "" [] (queryOptsFromOpts nulldate defreportopts{begin_=Just (parsedate "2012/01/01")
                                                             ,query_="date:'to 2013'"
                                                             })
 ]


tests_Hledger_Reports_ReportOptions :: Test
tests_Hledger_Reports_ReportOptions = TestList $
    tests_queryFromOpts
 ++ tests_queryOptsFromOpts
