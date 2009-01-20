{-# OPTIONS_GHC -cpp #-}
module Options 
where
import System
import System.Console.GetOpt
import System.Directory
import System.Environment
import Text.Printf
import Data.Char (toLower)
import Ledger.Parse
import Ledger.Utils
import Ledger.Types
import Ledger.Dates


configflags = [
#ifdef VTY
  "vty"
#endif
#ifdef ANSI
 ,"ansi"
#endif
#ifdef HAPPS
 ,"happs"
#endif
 ]
versionmsg  = "hledger " ++ version ++ configmsg ++ "\n"
version     = "0.3.x"
configmsg = if null configflags
            then ""
            else " with " ++ intercalate ", " configflags
ledgerdefault  = "~/.ledger"
ledgerenvvar   = "LEDGER"
timelogdefault = "~/.timelog"
timelogenvvar  = "TIMELOG"
timeprogname   = "hours"
usagehdr    = "Usage: hledger [OPTION] COMMAND [ACCTPATTERNS] [-- DESCPATTERNS]\n" ++
              "or:    hours [OPTIONS] [PERIOD [COMMAND [PATTERNS]]]\n" ++
              "\n" ++
              "Commands (can be abbreviated):\n" ++
              "  balance  - show account balances\n" ++
              "  print    - show formatted ledger entries\n" ++
              "  register - show register transactions\n" ++
#ifdef VTY
              "  ui       - run a simple vty-based text ui\n" ++
#endif
#ifdef ANSI
              "  ansi     - run a simple ansi-based text ui\n" ++
#endif
#ifdef HAPPS
              "  happs    - run a web server providing a minimal web ui\n" ++
#endif
              "\n" ++
              "Options (before command, unless using --options-anywhere):"
usageftr    = "\n" ++
              "All dates can be y/m/d or ledger-style smart dates like \"last month\".\n" ++
              "\n" ++
              "Account and description patterns are regular expressions which filter by\n" ++
              "account name and entry description. Prefix a pattern with - to negate it,\n" ++
              "and separate account and description patterns with --.\n" ++
              "(With --options-anywhere, use ^ and ^^. \"hours\" implies --options-anywhere.)\n" ++
              "\n" ++
              "Also: hledger [-v] test [TESTPATTERNS] to run self-tests.\n" ++
              "\n"
usage = usageInfo usagehdr options ++ usageftr

-- | Command-line options we accept.
options :: [OptDescr Opt]
options = [
  Option ['f'] ["file"]         (ReqArg File "FILE")   filehelp
 ,Option ['b'] ["begin"]        (ReqArg Begin "DATE")  "report on entries on or after this date"
 ,Option ['e'] ["end"]          (ReqArg End "DATE")    "report on entries prior to this date"
 ,Option ['p'] ["period"]       (ReqArg Period "EXPR") ("report on entries during the specified period\n" ++
                                                       "and/or with the specified reporting interval\n")
 ,Option ['C'] ["cleared"]      (NoArg  Cleared)       "report only on cleared entries"
 ,Option ['B'] ["cost","basis"] (NoArg  CostBasis)     "report cost basis of commodities"
 ,Option []    ["depth"]        (ReqArg Depth "N")     "balance report: maximum account depth to show"
 ,Option ['d'] ["display"]      (ReqArg Display "EXPR") ("display only transactions matching simple EXPR\n" ++
                                                        "(where EXPR is 'dOP[DATE]', OP is <, <=, =, >=, >)")
 ,Option ['E'] ["empty"]        (NoArg  Empty)         "balance report: show accounts with zero balance"
 ,Option ['R'] ["real"]         (NoArg  Real)          "report only on real (non-virtual) transactions"
 ,Option ['n'] ["collapse"]     (NoArg  Collapse)      "balance report: no grand total"
 ,Option ['s'] ["subtotal"]     (NoArg  SubTotal)      "balance report: show subaccounts"
 ,Option ['W'] ["weekly"]       (NoArg  WeeklyOpt)     "register report: show weekly summary"
 ,Option ['M'] ["monthly"]      (NoArg  MonthlyOpt)    "register report: show monthly summary"
 ,Option ['Y'] ["yearly"]       (NoArg  YearlyOpt)     "register report: show yearly summary"
 ,Option ['h'] ["help"] (NoArg  Help)                  "show this help"
 ,Option ['v'] ["verbose"]      (NoArg  Verbose)       "verbose test output"
 ,Option ['V'] ["version"]      (NoArg  Version)       "show version"
 ,Option []    ["debug-no-ui"]  (NoArg  DebugNoUI)     "run ui commands without no output"
 ,Option []    ["options-anywhere"] (NoArg OptionsAnywhere) "allow options anywhere on the command line"
 ]
    where 
      filehelp = printf "ledger file; - means use standard input. Defaults\nto the %s environment variable or %s"
                 ledgerenvvar ledgerdefault

-- | An option value from a command-line flag.
data Opt = 
    File    {value::String} | 
    Begin   {value::String} | 
    End     {value::String} | 
    Period  {value::String} | 
    Cleared | 
    CostBasis | 
    Depth   {value::String} | 
    Display {value::String} | 
    Empty | 
    Real | 
    OptionsAnywhere | 
    Collapse |
    SubTotal |
    WeeklyOpt |
    MonthlyOpt |
    YearlyOpt |
    Help |
    Verbose |
    Version
    | DebugNoUI
    deriving (Show,Eq)

-- yow..
optsWithConstructor f opts = concatMap get opts
    where get o = if f v == o then [o] else [] where v = value o

optValuesForConstructor f opts = concatMap get opts
    where get o = if f v == o then [v] else [] where v = value o

optValuesForConstructors fs opts = concatMap get opts
    where get o = if any (\f -> f v == o) fs then [v] else [] where v = value o

-- | Parse the command-line arguments into options, command name, and
-- command arguments. Any dates in the options are converted to full
-- YYYY/MM/DD format, while we are in the IO monad and can get the current
-- time. Arguments are parsed differently if the program was invoked as
-- "hours".
parseArguments :: IO ([Opt], String, [String])
parseArguments = do
  args <- getArgs
  istimequery <- usingTimeProgramName
  let order = if "--options-anywhere" `elem` args || istimequery
              then Permute 
              else RequireOrder
  let (os,as,es) = getOpt order options args
  os' <- fixOptDates os
  case istimequery of
    False ->
        case (os,as,es) of
          (opts,cmd:args,[])   -> return (os',cmd,args)
          (opts,[],[])         -> return (os',"",[])
          (opts,_,errs)        -> ioError (userError (concat errs ++ usage))
    True -> 
        case (os,as,es) of
          (opts,p:cmd:args,[]) -> return (os' ++ [Period p],cmd,args)
          (opts,p:args,[])     -> return ([Period p,SubTotal] ++ os',"balance",args)
          (opts,[],[])         -> return ([Period "today",SubTotal] ++ os',"balance",[])
          (opts,_,errs)        -> ioError (userError (concat errs ++ usage))
      

-- | Convert any fuzzy dates within these option values to explicit ones,
-- based on today's date.
fixOptDates :: [Opt] -> IO [Opt]
fixOptDates opts = do
  t <- today
  return $ map (fixopt t) opts
  where
    fixopt t (Begin s)   = Begin $ fixSmartDateStr t s
    fixopt t (End s)     = End $ fixSmartDateStr t s
    fixopt t (Display s) = -- hacky
        Display $ gsubRegexPRBy "\\[.+?\\]" fixbracketeddatestr s
        where fixbracketeddatestr s = "[" ++ (fixSmartDateStr t $ init $ tail s) ++ "]"
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

-- | Get the value of the (last) depth option, if any.
depthFromOpts :: [Opt] -> Maybe Int
depthFromOpts opts = listtomaybeint $ optValuesForConstructor Depth opts
    where
      listtomaybeint [] = Nothing
      listtomaybeint vs = Just $ read $ last vs

-- | Get the value of the (last) display option, if any.
displayFromOpts :: [Opt] -> Maybe String
displayFromOpts opts = listtomaybe $ optValuesForConstructor Display opts
    where
      listtomaybe [] = Nothing
      listtomaybe vs = Just $ last vs

-- | Was the program invoked via the \"hours\" alias ?
usingTimeProgramName :: IO Bool
usingTimeProgramName = do
  progname <- getProgName
  return $ map toLower progname == timeprogname

-- | Get the ledger file path from options, an environment variable, or a default
ledgerFilePathFromOpts :: [Opt] -> IO String
ledgerFilePathFromOpts opts = do
  istimequery <- usingTimeProgramName
  let (e,d) = if istimequery
              then (timelogenvvar,timelogdefault)
              else (ledgerenvvar,ledgerdefault)
  envordefault <- getEnv e `catch` \_ -> return d
  paths <- mapM tildeExpand $ [envordefault] ++ optValuesForConstructor File opts
  return $ last paths

-- | Expand ~ in a file path (does not handle ~name).
tildeExpand :: FilePath -> IO FilePath
tildeExpand ('~':[])     = getHomeDirectory
tildeExpand ('~':'/':xs) = getHomeDirectory >>= return . (++ ('/':xs))
--handle ~name, requires -fvia-C or ghc 6.8:
--import System.Posix.User
-- tildeExpand ('~':xs)     =  do let (user, path) = span (/= '/') xs
--                                pw <- getUserEntryForName user
--                                return (homeDirectory pw ++ path)
tildeExpand xs           =  return xs

-- | Gather any ledger-style account/description pattern arguments into
-- two lists.  These are 0 or more account patterns optionally followed by
-- a separator and then 0 or more description patterns. The separator is
-- usually -- but with --options-anywhere is ^^ so we need to provide the
-- options as well.
parseAccountDescriptionArgs :: [Opt] -> [String] -> ([String],[String])
parseAccountDescriptionArgs opts args = (as, ds')
    where (as, ds) = break (==patseparator) args
          ds' = dropWhile (==patseparator) ds
          patseparator = replicate 2 negchar
          negchar
              | OptionsAnywhere `elem` opts = '^'
              | otherwise = '-'
