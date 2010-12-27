{-# LANGUAGE CPP #-}
{-|
Command-line options for the application.
-}

module Hledger.Cli.Options
where
import Codec.Binary.UTF8.String (decodeString)
import System.Console.GetOpt
import System.Environment

import Hledger.Data.Utils
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Read (myJournalPath, myTimelogPath)


progname_cli = "hledger"

-- | The program name which, if we are invoked as (via symlink or
-- renaming), causes us to default to reading the user's time log instead
-- of their journal.
progname_cli_time  = "hours"

usage_preamble_cli =
  "Usage: hledger [OPTIONS] COMMAND [PATTERNS]\n" ++
  "       hledger [OPTIONS] convert CSVFILE\n" ++
  "\n" ++
  "Reads your ~/.journal file, or another specified by $LEDGER or -f, and\n" ++
  "runs the specified command (may be abbreviated):\n" ++
  "\n" ++
  "  add       - prompt for new transactions and add them to the journal\n" ++
  "  balance   - show accounts, with balances\n" ++
  "  convert   - show the specified CSV file as a hledger journal\n" ++
  "  histogram - show a barchart of transactions per day or other interval\n" ++
  "  print     - show transactions in journal format\n" ++
  "  register  - show transactions as a register with running balance\n" ++
  "  stats     - show various statistics for a journal\n" ++
  "  test      - run self-tests\n" ++
  "\n"

usage_options_cli = usageInfo "hledger options:" options_cli

usage_postscript_cli =
 "\n" ++
 "DATES can be y/m/d or smart dates like \"last month\".  PATTERNS are regular\n" ++
 "expressions which filter by account name.  Prefix a pattern with desc: to\n" ++
 "filter by transaction description instead, prefix with not: to negate it.\n" ++
 "When using both, not: comes last.\n"

usage_cli = concat [
             usage_preamble_cli
            ,usage_options_cli
            ,usage_postscript_cli
            ]

-- | Command-line options we accept.
options_cli :: [OptDescr Opt]
options_cli = [
  Option "f" ["file"]         (ReqArg File "FILE")   "use a different journal/timelog file; - means stdin"
 ,Option ""  ["no-new-accounts"] (NoArg NoNewAccts)  "don't allow to create new accounts"
 ,Option "b" ["begin"]        (ReqArg Begin "DATE")  "report on transactions on or after this date"
 ,Option "e" ["end"]          (ReqArg End "DATE")    "report on transactions before this date"
 ,Option "p" ["period"]       (ReqArg Period "EXPR") ("report on transactions during the specified period\n" ++
                                                      "and/or with the specified reporting interval\n")
 ,Option "C" ["cleared"]      (NoArg  Cleared)       "report only on cleared transactions"
 ,Option "U" ["uncleared"]    (NoArg  UnCleared)     "report only on uncleared transactions"
 ,Option "B" ["cost","basis"] (NoArg  CostBasis)     "report cost of commodities"
 ,Option ""  ["depth"]        (ReqArg Depth "N")     "hide accounts/transactions deeper than this"
 ,Option "d" ["display"]      (ReqArg Display "EXPR") ("show only transactions matching EXPR (where\n" ++
                                                       "EXPR is 'dOP[DATE]' and OP is <, <=, =, >=, >)")
 ,Option ""  ["effective"]    (NoArg  Effective)     "use transactions' effective dates, if any"
 ,Option "E" ["empty"]        (NoArg  Empty)         "show empty/zero things which are normally elided"
 ,Option "R" ["real"]         (NoArg  Real)          "report only on real (non-virtual) transactions"
 ,Option ""  ["flat"]         (NoArg  Flat)          "balance: show full account names, unindented"
 ,Option ""  ["drop"]         (ReqArg Drop "N")      "balance: with --flat, elide first N account name components"
 ,Option ""  ["no-total"]     (NoArg  NoTotal)       "balance: hide the final total"
 ,Option "D" ["daily"]        (NoArg  DailyOpt)      "register, stats: report by day"
 ,Option "W" ["weekly"]       (NoArg  WeeklyOpt)     "register, stats: report by week"
 ,Option "M" ["monthly"]      (NoArg  MonthlyOpt)    "register, stats: report by month"
 ,Option "Q" ["quarterly"]    (NoArg  QuarterlyOpt)  "register, stats: report by quarter"
 ,Option "Y" ["yearly"]       (NoArg  YearlyOpt)     "register, stats: report by year"
 ,Option "v" ["verbose"]      (NoArg  Verbose)       "show more verbose output"
 ,Option ""  ["debug"]        (NoArg  Debug)         "show extra debug output; implies verbose"
 ,Option ""  ["binary-filename"] (NoArg BinaryFilename) "show the download filename for this hledger build"
 ,Option "V" ["version"]      (NoArg  Version)       "show version information"
 ,Option "h" ["help"]         (NoArg  Help)          "show command-line usage"
 ]

-- | An option value from a command-line flag.
data Opt = 
    File          {value::String}
    | NoNewAccts
    | Begin       {value::String}
    | End         {value::String}
    | Period      {value::String}
    | Cleared
    | UnCleared
    | CostBasis
    | Depth       {value::String}
    | Display     {value::String}
    | Effective
    | Empty
    | Real
    | Flat
    | Drop        {value::String}
    | NoTotal
    | SubTotal
    | DailyOpt
    | WeeklyOpt
    | MonthlyOpt
    | QuarterlyOpt
    | YearlyOpt
    | Help
    | Verbose
    | Version
    | BinaryFilename
    | Debug
    -- XXX add-on options, must be defined here for now
    -- vty
    | DebugVty
    -- web
    | BaseUrl     {value::String}
    | Port        {value::String}
    -- chart
    | ChartOutput {value::String}
    | ChartItems  {value::String}
    | ChartSize   {value::String}
    deriving (Show,Eq)

-- these make me nervous
optsWithConstructor f opts = concatMap get opts
    where get o = [o | f v == o] where v = value o

optsWithConstructors fs opts = concatMap get opts
    where get o = [o | any (== o) fs]

optValuesForConstructor f opts = concatMap get opts
    where get o = [v | f v == o] where v = value o

optValuesForConstructors fs opts = concatMap get opts
    where get o = [v | any (\f -> f v == o) fs] where v = value o

-- | Parse the command-line arguments into options and arguments using the
-- specified option descriptors. Any smart dates in the options are
-- converted to explicit YYYY/MM/DD format based on the current time. If
-- parsing fails, raise an error, displaying the problem along with the
-- provided usage string.
parseArgumentsWith :: [OptDescr Opt] -> IO ([Opt], [String])
parseArgumentsWith options = do
  rawargs <- map decodeString `fmap` getArgs
  let (opts,args,errs) = getOpt Permute options rawargs
  opts' <- fixOptDates opts
  let opts'' = if Debug `elem` opts' then Verbose:opts' else opts'
  if null errs
   then return (opts'',args)
   else argsError (concat errs) >> return ([],[])

argsError :: String -> IO ()
argsError = ioError . userError' . (++ " Run with --help to see usage.")

-- | Convert any fuzzy dates within these option values to explicit ones,
-- based on today's date.
fixOptDates :: [Opt] -> IO [Opt]
fixOptDates opts = do
  d <- getCurrentDay
  return $ map (fixopt d) opts
  where
    fixopt d (Begin s)   = Begin $ fixSmartDateStr d s
    fixopt d (End s)     = End $ fixSmartDateStr d s
    fixopt d (Display s) = -- hacky
        Display $ gsubRegexPRBy "\\[.+?\\]" fixbracketeddatestr s
        where fixbracketeddatestr s = "[" ++ fixSmartDateStr d (init $ tail s) ++ "]"
    fixopt _ o            = o

-- | Figure out the overall date span we should report on, based on any
-- begin/end/period options provided. If there is a period option, the
-- others are ignored.
dateSpanFromOpts :: Day -> [Opt] -> DateSpan
dateSpanFromOpts refdate opts
    | not (null popts) = case parsePeriodExpr refdate $ last popts of
                         Right (_, s) -> s
                         Left e       -> parseerror e
    | otherwise = DateSpan lastb laste
    where
      popts = optValuesForConstructor Period opts
      bopts = optValuesForConstructor Begin opts
      eopts = optValuesForConstructor End opts
      lastb = listtomaybeday bopts
      laste = listtomaybeday eopts
      listtomaybeday vs = if null vs then Nothing else Just $ parse $ last vs
          where parse = parsedate . fixSmartDateStr refdate

-- | Figure out the reporting interval, if any, specified by the options.
-- If there is a period option, the others are ignored.
intervalFromOpts :: [Opt] -> Interval
intervalFromOpts opts =
    case (periodopts, intervalopts) of
      ((p:_), _)            -> case parsePeriodExpr (parsedate "0001/01/01") p of
                                Right (i, _) -> i
                                Left e       -> parseerror e
      (_, (DailyOpt:_))     -> Daily
      (_, (WeeklyOpt:_))    -> Weekly
      (_, (MonthlyOpt:_))   -> Monthly
      (_, (QuarterlyOpt:_)) -> Quarterly
      (_, (YearlyOpt:_))    -> Yearly
      (_, _)                -> NoInterval
    where
      periodopts   = reverse $ optValuesForConstructor Period opts
      intervalopts = reverse $ filter (`elem` [DailyOpt,WeeklyOpt,MonthlyOpt,QuarterlyOpt,YearlyOpt]) opts

-- | Get the value of the (last) depth option, if any.
depthFromOpts :: [Opt] -> Maybe Int
depthFromOpts opts = listtomaybeint $ optValuesForConstructor Depth opts
    where
      listtomaybeint [] = Nothing
      listtomaybeint vs = Just $ read $ last vs

-- | Get the value of the (last) drop option, if any, otherwise 0.
dropFromOpts :: [Opt] -> Int
dropFromOpts opts = fromMaybe 0 $ listtomaybeint $ optValuesForConstructor Drop opts
    where
      listtomaybeint [] = Nothing
      listtomaybeint vs = Just $ read $ last vs

-- | Get the value of the (last) display option, if any.
displayExprFromOpts :: [Opt] -> Maybe String
displayExprFromOpts opts = listtomaybe $ optValuesForConstructor Display opts
    where
      listtomaybe [] = Nothing
      listtomaybe vs = Just $ last vs

-- | Get the value of the (last) baseurl option, if any.
baseUrlFromOpts :: [Opt] -> Maybe String
baseUrlFromOpts opts = listtomaybe $ optValuesForConstructor BaseUrl opts
    where
      listtomaybe [] = Nothing
      listtomaybe vs = Just $ last vs

-- | Get the value of the (last) port option, if any.
portFromOpts :: [Opt] -> Maybe Int
portFromOpts opts = listtomaybeint $ optValuesForConstructor Port opts
    where
      listtomaybeint [] = Nothing
      listtomaybeint vs = Just $ read $ last vs


-- | Get a maybe boolean representing the last cleared/uncleared option if any.
clearedValueFromOpts opts | null os = Nothing
                          | last os == Cleared = Just True
                          | otherwise = Just False
    where os = optsWithConstructors [Cleared,UnCleared] opts

-- | Were we invoked as \"hours\" ?
usingTimeProgramName :: IO Bool
usingTimeProgramName = do
  progname <- getProgName
  return $ map toLower progname == progname_cli_time

-- | Get the journal file path from options, an environment variable, or a default
journalFilePathFromOpts :: [Opt] -> IO String
journalFilePathFromOpts opts = do
  istimequery <- usingTimeProgramName
  f <- if istimequery then myTimelogPath else myJournalPath
  return $ last $ f : optValuesForConstructor File opts

-- | Gather filter pattern arguments into a list of account patterns and a
-- list of description patterns. We interpret pattern arguments as
-- follows: those prefixed with "desc:" are description patterns, all
-- others are account patterns; also patterns prefixed with "not:" are
-- negated. not: should come after desc: if both are used.
parsePatternArgs :: [String] -> ([String],[String])
parsePatternArgs args = (as, ds')
    where
      descprefix = "desc:"
      (ds, as) = partition (descprefix `isPrefixOf`) args
      ds' = map (drop (length descprefix)) ds

-- | Convert application options to the library's generic filter specification.
optsToFilterSpec :: [Opt] -> [String] -> LocalTime -> FilterSpec
optsToFilterSpec opts args t = FilterSpec {
                                datespan=dateSpanFromOpts (localDay t) opts
                               ,cleared=clearedValueFromOpts opts
                               ,real=Real `elem` opts
                               ,empty=Empty `elem` opts
                               ,costbasis=CostBasis `elem` opts
                               ,acctpats=apats
                               ,descpats=dpats
                               ,whichdate = if Effective `elem` opts then EffectiveDate else ActualDate
                               ,depth = depthFromOpts opts
                               }
    where (apats,dpats) = parsePatternArgs args

-- currentLocalTimeFromOpts opts = listtomaybe $ optValuesForConstructor CurrentLocalTime opts
--     where
--       listtomaybe [] = Nothing
--       listtomaybe vs = Just $ last vs

tests_Hledger_Cli_Options = TestList
 [
  "dateSpanFromOpts" ~: do
    let todaysdate = parsedate "2008/11/26"
    let gives = is . show . dateSpanFromOpts todaysdate
    [] `gives` "DateSpan Nothing Nothing"
    [Begin "2008", End "2009"] `gives` "DateSpan (Just 2008-01-01) (Just 2009-01-01)"
    [Period "in 2008"] `gives` "DateSpan (Just 2008-01-01) (Just 2009-01-01)"
    [Begin "2005", End "2007",Period "in 2008"] `gives` "DateSpan (Just 2008-01-01) (Just 2009-01-01)"

  ,"intervalFromOpts" ~: do
    let gives = is . intervalFromOpts
    [] `gives` NoInterval
    [DailyOpt] `gives` Daily
    [WeeklyOpt] `gives` Weekly
    [MonthlyOpt] `gives` Monthly
    [QuarterlyOpt] `gives` Quarterly
    [YearlyOpt] `gives` Yearly
    [Period "weekly"] `gives` Weekly
    [Period "monthly"] `gives` Monthly
    [Period "quarterly"] `gives` Quarterly
    [WeeklyOpt, Period "yearly"] `gives` Yearly

 ]