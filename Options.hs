{-# OPTIONS_GHC -cpp #-}
{-|
Command-line options for the application.
-}

module Options 
where
import System
import System.Console.GetOpt
import System.Environment
import Text.Printf
import Text.RegexPR (gsubRegexPRBy)
import Data.Char (toLower)
import Ledger.IO (IOArgs,
                  ledgerenvvar,ledgerdefaultpath,myLedgerPath,
                  timelogenvvar,timelogdefaultpath,myTimelogPath)
import Ledger.Parse
import Ledger.Utils
import Ledger.Types
import Ledger.Dates

progname      = "hledger"
timeprogname  = "hours"

usagehdr = (
  "Usage: hledger [OPTIONS] [COMMAND [PATTERNS]]\n" ++
  "       hours   [OPTIONS] [COMMAND [PATTERNS]]\n" ++
  "\n" ++
  "When invoked as \"hours\", uses your timelog and --period today as defaults.\n" ++
  "\n" ++
  "COMMAND is one of (may be abbreviated):\n" ++
  "  add       - read new transactions interactively\n" ++
  "  balance   - show account balances\n" ++
  "  histogram - show transaction counts per reporting interval\n" ++
  "  print     - show formatted ledger transactions\n" ++
  "  register  - show register transactions\n" ++
#ifdef VTY
  "  ui        - run a simple curses-based text ui\n" ++
#endif
#ifdef HAPPS
  "  web       - run a simple web ui\n" ++
#endif
  "  test      - run self-tests\n" ++
  "\n" ++
  "PATTERNS are regular expressions which filter by account name.\n" ++
  "Or, prefix with desc: to filter by entry description.\n" ++
  "Or, prefix with not: to negate a pattern. (When using both, not: comes last.)\n" ++
  "\n" ++
  "Dates can be y/m/d or ledger-style smart dates like \"last month\".\n" ++
  "\n" ++
  "Options:"
  )
  

usageftr = printf (
  "\n"
  )

usage = usageInfo usagehdr options ++ usageftr

-- | Command-line options we accept.
options :: [OptDescr Opt]
options = [
  Option ['f'] ["file"]         (ReqArg File "FILE")   filehelp
 ,Option ['b'] ["begin"]        (ReqArg Begin "DATE")  "report on transactions on or after this date"
 ,Option ['e'] ["end"]          (ReqArg End "DATE")    "report on transactions prior to this date"
 ,Option ['p'] ["period"]       (ReqArg Period "EXPR") ("report on transactions during the specified period\n" ++
                                                       "and/or with the specified reporting interval\n")
 ,Option ['C'] ["cleared"]      (NoArg  Cleared)       "report only on cleared transactions"
 ,Option ['U'] ["uncleared"]    (NoArg  UnCleared)     "report only on uncleared transactions"
 ,Option ['B'] ["cost","basis"] (NoArg  CostBasis)     "report cost of commodities"
 ,Option []    ["depth"]        (ReqArg Depth "N")     "hide accounts/transactions deeper than this"
 ,Option ['d'] ["display"]      (ReqArg Display "EXPR") ("show only transactions matching EXPR (where\n" ++
                                                        "EXPR is 'dOP[DATE]' and OP is <, <=, =, >=, >)")
 ,Option ['E'] ["empty"]        (NoArg  Empty)         "show empty/zero things which are normally elided"
 ,Option ['R'] ["real"]         (NoArg  Real)          "report only on real (non-virtual) transactions"
 ,Option []    ["no-total"]     (NoArg  NoTotal)       "balance report: hide the final total"
-- ,Option ['s'] ["subtotal"]     (NoArg  SubTotal)      "balance report: show subaccounts"
 ,Option ['W'] ["weekly"]       (NoArg  WeeklyOpt)     "register report: show weekly summary"
 ,Option ['M'] ["monthly"]      (NoArg  MonthlyOpt)    "register report: show monthly summary"
 ,Option ['Y'] ["yearly"]       (NoArg  YearlyOpt)     "register report: show yearly summary"
 ,Option ['h'] ["help"] (NoArg  Help)                  "show this help"
 ,Option ['V'] ["version"]      (NoArg  Version)       "show version information"
 ,Option ['v'] ["verbose"]      (NoArg  Verbose)       "show verbose test output"
 ,Option []    ["debug"]        (NoArg  Debug)         "show some debug output"
 ,Option []    ["debug-no-ui"]  (NoArg  DebugNoUI)     "run ui commands with no output"
 ]
    where 
      filehelp = printf (intercalate "\n"
                         ["ledger file; default is the %s env. variable's"
                         ,"value, or %s. - means use standard input."
                         ]) ledgerenvvar ledgerdefaultpath

-- | An option value from a command-line flag.
data Opt = 
    File    {value::String} | 
    Begin   {value::String} | 
    End     {value::String} | 
    Period  {value::String} | 
    Cleared | 
    UnCleared | 
    CostBasis | 
    Depth   {value::String} | 
    Display {value::String} | 
    Empty | 
    Real | 
    NoTotal |
    SubTotal |
    WeeklyOpt |
    MonthlyOpt |
    YearlyOpt |
    Help |
    Verbose |
    Version
    | Debug
    | DebugNoUI
    deriving (Show,Eq)

-- these make me nervous
optsWithConstructor f opts = concatMap get opts
    where get o = if f v == o then [o] else [] where v = value o

optsWithConstructors fs opts = concatMap get opts
    where get o = if any (\f -> f == o) fs then [o] else []

optValuesForConstructor f opts = concatMap get opts
    where get o = if f v == o then [v] else [] where v = value o

optValuesForConstructors fs opts = concatMap get opts
    where get o = if any (\f -> f v == o) fs then [v] else [] where v = value o

-- | Parse the command-line arguments into options, command name, and
-- command arguments. Any dates in the options are converted to explicit
-- YYYY/MM/DD format based on the current time. If the program was invoked
-- as \"hours\", the -f $TIMELOG -p today options are assumed as a default.
parseArguments :: IO ([Opt], String, [String])
parseArguments = do
  args <- getArgs
  let (os,as,es) = getOpt Permute options args
  istimequery <- usingTimeProgramName
  let os' = if istimequery then (Period "today"):os else os
  os'' <- fixOptDates os'
  case (as,es) of
    (cmd:args,[])   -> return (os'',cmd,args)
    ([],[])         -> return (os'',"",[])
    (_,errs)        -> ioError (userError (concat errs ++ usage))

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
        where fixbracketeddatestr s = "[" ++ (fixSmartDateStr d $ init $ tail s) ++ "]"
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
intervalFromOpts opts
    | not $ null popts = fst $ parsePeriodExpr refdate $ last popts
    | null otheropts = NoInterval
    | otherwise = case last otheropts of
                    WeeklyOpt  -> Weekly
                    MonthlyOpt -> Monthly
                    YearlyOpt  -> Yearly
    where
      popts = optValuesForConstructor Period opts
      otheropts = filter (`elem` [WeeklyOpt,MonthlyOpt,YearlyOpt]) opts 
      -- doesn't affect the interval, but parsePeriodExpr needs something
      refdate = parsedate "0001/01/01"

-- | Get the value of the (last) depth option, if any, otherwise a large number.
depthFromOpts :: [Opt] -> Int
depthFromOpts opts = fromMaybe 9999 $ listtomaybeint $ optValuesForConstructor Depth opts
    where
      listtomaybeint [] = Nothing
      listtomaybeint vs = Just $ read $ last vs

-- | Get the value of the (last) display option, if any.
displayFromOpts :: [Opt] -> Maybe String
displayFromOpts opts = listtomaybe $ optValuesForConstructor Display opts
    where
      listtomaybe [] = Nothing
      listtomaybe vs = Just $ last vs

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

-- | Get the ledger file path from options, an environment variable, or a default
ledgerFilePathFromOpts :: [Opt] -> IO String
ledgerFilePathFromOpts opts = do
  istimequery <- usingTimeProgramName
  f <- if istimequery then myTimelogPath else myLedgerPath
  return $ last $ f:(optValuesForConstructor File opts)

-- | Gather any pattern arguments into a list of account patterns and a
-- list of description patterns. For now we interpret pattern arguments as
-- follows: those prefixed with "desc:" are description patterns, all
-- others are account patterns. Also patterns prefixed with "not:" are
-- negated. not: should come after desc: if both are used.
-- This is different from ledger 2 and 3.
parseAccountDescriptionArgs :: [Opt] -> [String] -> ([String],[String])
parseAccountDescriptionArgs opts args = (as, ds')
    where
      descprefix = "desc:"
      (ds, as) = partition (descprefix `isPrefixOf`) args
      ds' = map (drop (length descprefix)) ds

-- | Convert application options to more generic types for the library.
optsToIOArgs :: [Opt] -> [String] -> LocalTime -> IOArgs
optsToIOArgs opts args t = (dateSpanFromOpts (localDay t) opts
                         ,clearedValueFromOpts opts
                         ,Real `elem` opts
                         ,CostBasis `elem` opts
                         ,apats
                         ,dpats
                         ) where (apats,dpats) = parseAccountDescriptionArgs [] args

