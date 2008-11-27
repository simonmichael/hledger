module Options 
where
import System
import System.Console.GetOpt
import System.Directory
import Text.Printf
import Ledger.Parse
import Ledger.Utils
import Ledger.Types
import Ledger.Dates


defaultfile = "~/.ledger"
fileenvvar  = "LEDGER"
usagehdr    = "Usage: hledger [OPTS] COMMAND [ACCTPATTERNS] [-- DESCPATTERNS]\n" ++
              "\n" ++
              "Options (before command, unless using --options-anywhere):"
usageftr    = "\n" ++
              "Commands (may be abbreviated):\n" ++
              "  balance  - show account balances\n" ++
              "  print    - show formatted ledger entries\n" ++
              "  register - show register transactions\n" ++
              "\n" ++
              "Account and description patterns are regular expressions which filter by\n" ++
              "account name and entry description. Prefix a pattern with - to negate it,\n" ++
              "and separate account and description patterns with --.\n" ++
              "(With --options-anywhere, use ^ and ^^.)\n" ++
              "\n" ++
              "Also: hledger [-v] test [TESTPATTERNS] to run self-tests.\n" ++
              "\n"
usage = usageInfo usagehdr options ++ usageftr

-- | Command-line options we accept.
options :: [OptDescr Opt]
options = [
 Option ['f'] ["file"]         (ReqArg File "FILE")   filehelp,
 Option ['b'] ["begin"]        (ReqArg Begin "Y/M/D") "report on entries on or after this date",
 Option ['e'] ["end"]          (ReqArg End "Y/M/D")   "report on entries prior to this date",
 Option ['C'] ["cleared"]      (NoArg  Cleared)       "report only on cleared entries",
 Option ['B'] ["cost","basis"] (NoArg  CostBasis)     "report cost basis of commodities",
 Option []    ["depth"]        (ReqArg Depth "N")     "balance report: maximum account depth to show",
 Option ['d'] ["display"]      (ReqArg Display "EXPR") ("display only transactions matching simple EXPR\n" ++
                                                        "(where EXPR is 'dOP[Y/M/D]', OP is <, <=, =, >=, >)"),
 Option ['E'] ["empty"]        (NoArg  Empty)         "balance report: show accounts with zero balance",
 Option ['R'] ["real"]         (NoArg  Real)          "report only on real (non-virtual) transactions",
 Option []    ["options-anywhere"] (NoArg OptionsAnywhere) "allow options anywhere, use ^ to negate patterns",
 Option ['n'] ["collapse"]     (NoArg  Collapse)      "balance report: no grand total",
 Option ['s'] ["subtotal"]     (NoArg  SubTotal)      "balance report: show subaccounts",
 Option ['h'] ["help"] (NoArg  Help)                  "show this help",
 Option ['v'] ["verbose"]      (NoArg  Verbose)       "verbose test output",
 Option ['V'] ["version"]      (NoArg  Version)       "show version"
 ]
    where 
      filehelp = printf "ledger file; - means use standard input. Defaults\nto the %s environment variable or %s"
                 fileenvvar defaultfile

-- | An option value from a command-line flag.
data Opt = 
    File String | 
    Begin String | 
    End String | 
    Cleared | 
    CostBasis | 
    Depth String | 
    Display String | 
    Empty | 
    Real | 
    OptionsAnywhere | 
    Collapse |
    SubTotal |
    Help |
    Verbose |
    Version
    deriving (Show,Eq)

versionno = "0.3pre"
version = printf "hledger version %s \n" versionno :: String

-- | Parse the command-line arguments into ledger options, ledger command
-- name, and ledger command arguments. Also any dates in the options are
-- converted to full YYYY/MM/DD format, while we are in the IO monad
-- and can get the current time.
parseArguments :: IO ([Opt], String, [String])
parseArguments = do
  args <- getArgs
  let order = if "--options-anywhere" `elem` args then Permute else RequireOrder
  case (getOpt order options args) of
    (opts,cmd:args,[]) -> do {opts' <- fixOptDates opts; return (opts',cmd,args)}
    (opts,[],[])       -> do {opts' <- fixOptDates opts; return (opts',[],[])}
    (opts,_,errs)      -> ioError (userError (concat errs ++ usage))

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

-- | Get the ledger file path from options, an environment variable, or a default
ledgerFilePathFromOpts :: [Opt] -> IO String
ledgerFilePathFromOpts opts = do
  envordefault <- getEnv fileenvvar `catch` \_ -> return defaultfile
  paths <- mapM tildeExpand $ [envordefault] ++ (concatMap getfile opts)
  return $ last paths
    where
      getfile (File s) = [s]
      getfile _ = []

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

-- | Get the value of the begin date option, if any.
beginDateFromOpts :: [Opt] -> Maybe Day
beginDateFromOpts opts =
    if null beginopts 
    then Nothing
    else Just $ parsedate $ printf "%04s/%02s/%02s" y m d
    where
      beginopts = concatMap getbegindate opts
      getbegindate (Begin s) = [s]
      getbegindate _ = []
      defaultdate = ""
      (y,m,d) = fromparse $ parsewith smartdate $ last beginopts

-- | Get the value of the end date option, if any.
endDateFromOpts :: [Opt] -> Maybe Day
endDateFromOpts opts =
    if null endopts 
    then Nothing
    else Just $ parsedate $ printf "%04s/%02s/%02s" y m d
    where
      endopts = concatMap getenddate opts
      getenddate (End s) = [s]
      getenddate _ = []
      defaultdate = ""
      (y,m,d) = fromparse $ parsewith smartdate $ last endopts

-- | Get the value of the depth option, if any.
depthFromOpts :: [Opt] -> Maybe Int
depthFromOpts opts =
    case depthopts of
      (x:_) -> Just $ read x
      _     -> Nothing
    where
      depthopts = concatMap getdepth opts
      getdepth (Depth s) = [s]
      getdepth _ = []

-- | Get the value of the display option, if any.
displayFromOpts :: [Opt] -> Maybe String
displayFromOpts opts =
    case displayopts of
      (s:_) -> Just s
      _     -> Nothing
    where
      displayopts = concatMap getdisplay opts
      getdisplay (Display s) = [s]
      getdisplay _ = []

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

testoptions order cmdline = putStr $ 
    case getOpt order options cmdline of
      (o,n,[]  ) -> "options=" ++ show o ++ "  args=" ++ show n
      (o,_,errs) -> concat errs ++ usage

