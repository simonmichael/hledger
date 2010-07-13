{-# LANGUAGE CPP #-}
{-|
Command-line options for the application.
-}

module Hledger.Cli.Options
where
import System.Console.GetOpt
import System.Environment
import Hledger.Cli.Version (timeprogname)
import Hledger.Read (myLedgerPath,myTimelogPath)
import Hledger.Data.Utils
import Hledger.Data.Types
import Hledger.Data.Dates
import Codec.Binary.UTF8.String (decodeString)

#ifdef CHART
chartoutput   = "hledger.png"
chartitems    = 10
chartsize     = "600x400"
#endif

help1 =
  "Usage: hledger [OPTIONS] COMMAND [PATTERNS]\n" ++
  "       hledger [OPTIONS] convert CSVFILE\n" ++
  "       hledger [OPTIONS] stats\n" ++
  "\n" ++
  "hledger reads your ~/.ledger file, or another specified with $LEDGER or -f\n" ++
  "\n" ++
  "COMMAND is one of (may be abbreviated):\n" ++
  "  add       - prompt for new transactions and add them to the ledger\n" ++
  "  balance   - show accounts, with balances\n" ++
  "  convert   - read CSV bank data and display in ledger format\n" ++
  "  histogram - show a barchart of transactions per day or other interval\n" ++
  "  print     - show transactions in ledger format\n" ++
  "  register  - show transactions as a register with running balance\n" ++
  "  stats     - show various statistics for a ledger\n" ++
  "  vty       - run a simple curses-style UI" ++
#ifdef VTY
  "\n" ++
#else
  " (DISABLED, install with -fvty)\n" ++
#endif
  "  web       - run a simple web-based UI" ++
#if defined(WEB) || defined(WEB610)
  "\n" ++
#else
  " (DISABLED, install with -fweb or -fweb610)\n" ++
#endif
  "  chart     - generate balances pie charts" ++
#ifdef CHART
  "\n" ++
#else
  " (DISABLED, install with -fchart)\n" ++
#endif
  "  test      - run self-tests\n" ++
  "\n" ++
  "PATTERNS are regular expressions which filter by account name.\n" ++
  "Prefix with desc: to filter by transaction description instead.\n" ++
  "Prefix with not: to negate a pattern. When using both, not: comes last.\n" ++
  "\n" ++
  "DATES can be y/m/d or ledger-style smart dates like \"last month\".\n" ++
  "\n" ++
  "Use --help-options to see OPTIONS, or --help-all/-H.\n" ++
  ""

help2 = usageInfo "Options:\n" options

-- | Command-line options we accept.
options :: [OptDescr Opt]
options = [
  Option "f" ["file"]         (ReqArg File "FILE")   "use a different ledger/timelog file; - means stdin"
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
 ,Option "W" ["weekly"]       (NoArg  WeeklyOpt)     "register, stats: report by week"
 ,Option "M" ["monthly"]      (NoArg  MonthlyOpt)    "register, stats: report by month"
 ,Option "Q" ["quarterly"]    (NoArg  QuarterlyOpt)  "register, stats: report by quarter"
 ,Option "Y" ["yearly"]       (NoArg  YearlyOpt)     "register, stats: report by year"
#ifdef CHART
 ,Option "o" ["output"]  (ReqArg ChartOutput "FILE")    ("chart: output filename (default: "++chartoutput++")")
 ,Option ""  ["items"]  (ReqArg ChartItems "N")         ("chart: number of accounts to show (default: "++show chartitems++")")
 ,Option ""  ["size"] (ReqArg ChartSize "WIDTHxHEIGHT") ("chart: image size (default: "++chartsize++")")
#endif
#ifdef VTY
 ,Option ""  ["debug-vty"]    (NoArg  DebugVty)      "vty: run with no terminal output, showing console"
#endif
#ifdef WEB
 ,Option ""  ["base-url"]     (ReqArg BaseUrl "URL") "web: use this base url (default http://localhost:PORT)"
 ,Option ""  ["port"]         (ReqArg Port "N")      "web: serve on tcp port N (default 5000)"
#endif
 ,Option "v" ["verbose"]      (NoArg  Verbose)       "show more verbose output"
 ,Option ""  ["debug"]        (NoArg  Debug)         "show extra debug output; implies verbose"
 ,Option ""  ["binary-filename"] (NoArg BinaryFilename) "show the download filename for this hledger build"
 ,Option "V" ["version"]      (NoArg  Version)       "show version information"
 ,Option "h" ["help"]         (NoArg  Help)          "show basic command-line usage"
 ,Option ""  ["help-options"] (NoArg  HelpOptions)   "show command-line options"
 ,Option "H" ["help-all"]     (NoArg  HelpAll)       "show command-line usage and options"
 ]

-- | An option value from a command-line flag.
data Opt = 
    File    {value::String} | 
    NoNewAccts |
    Begin   {value::String} | 
    End     {value::String} | 
    Period  {value::String} | 
    Cleared | 
    UnCleared | 
    CostBasis | 
    Depth   {value::String} | 
    Display {value::String} | 
    Effective | 
    Empty | 
    Real | 
    Flat |
    Drop   {value::String} |
    NoTotal |
    SubTotal |
    WeeklyOpt |
    MonthlyOpt |
    QuarterlyOpt |
    YearlyOpt |
#ifdef WEB
    BaseUrl {value::String} |
    Port    {value::String} |
#endif
    Help |
    HelpOptions |
    HelpAll |
    Verbose |
    Version
    | BinaryFilename
    | Debug
    | DebugVty
#ifdef CHART
    | ChartOutput {value::String}
    | ChartItems  {value::String}
    | ChartSize   {value::String}
#endif
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

-- | Parse the command-line arguments into options, command name, and
-- command arguments. Any dates in the options are converted to explicit
-- YYYY/MM/DD format based on the current time.
parseArguments :: IO ([Opt], String, [String])
parseArguments = do
  args <- liftM (map decodeString) getArgs
  let (os,as,es) = getOpt Permute options args
--  istimequery <- usingTimeProgramName
--  let os' = if istimequery then (Period "today"):os else os
  os' <- fixOptDates os
  let os'' = if Debug `elem` os' then Verbose:os' else os'
  case (as,es) of
    (cmd:args,[])   -> return (os'',cmd,args)
    ([],[])         -> return (os'',"",[])
    (_,errs)        -> ioError (userError (concat errs ++ help1))

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
    | not $ null popts = snd $ parsePeriodExpr refdate $ last popts
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
      ((p:_), _)            -> fst $ parsePeriodExpr d p where d = parsedate "0001/01/01" -- unused
      (_, (WeeklyOpt:_))    -> Weekly
      (_, (MonthlyOpt:_))   -> Monthly
      (_, (QuarterlyOpt:_)) -> Quarterly
      (_, (YearlyOpt:_))    -> Yearly
      (_, _)                -> NoInterval
    where
      periodopts   = reverse $ optValuesForConstructor Period opts
      intervalopts = reverse $ filter (`elem` [WeeklyOpt,MonthlyOpt,QuarterlyOpt,YearlyOpt]) opts

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

#ifdef WEB
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

#endif

-- | Get a maybe boolean representing the last cleared/uncleared option if any.
clearedValueFromOpts opts | null os = Nothing
                          | last os == Cleared = Just True
                          | otherwise = Just False
    where os = optsWithConstructors [Cleared,UnCleared] opts

-- | Was the program invoked via the \"hours\" alias ?
usingTimeProgramName :: IO Bool
usingTimeProgramName = do
  progname <- getProgName
  return $ map toLower progname == timeprogname

-- | Get the journal file path from options, an environment variable, or a default
journalFilePathFromOpts :: [Opt] -> IO String
journalFilePathFromOpts opts = do
  istimequery <- usingTimeProgramName
  f <- if istimequery then myTimelogPath else myLedgerPath
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

