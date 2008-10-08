module Options (
Opt(..), 
usage, 
parseArguments, 
ledgerFilePathFromOpts,
beginDateFromOpts,
endDateFromOpts,
parsePatternArgs, 
regexFor, 
nullpats, 
wildcard, 
)
where
import System
import System.Console.GetOpt
import System.Directory
import System.Environment (getEnv)
import Data.Maybe (fromMaybe)
    
import Ledger.Utils
import Ledger.Types
import Ledger.Parse (parseLedgerFile, parseError)
import Ledger.Ledger (cacheLedger)


usagehdr    = "Usage: hledger [OPTIONS] "++commands++" [ACCTPATTERNS] [-- DESCPATTERNS]\nOptions:"
commands    = "register|balance|print"
defaultcmd  = "register"
defaultfile = "~/.ledger"
fileenvvar  = "LEDGER"

-- | Command-line options we accept.
options :: [OptDescr Opt]
options = [
 Option ['f'] ["file"]         (ReqArg File "FILE")        "ledger file; - means use standard input",
 Option ['b'] ["begin"]        (ReqArg Begin "yyyy/mm/dd") "report on entries from this date (inclusive)",
 Option ['e'] ["end"]          (ReqArg End "yyyy/mm/dd")   "report on entries to this date (exclusive)",
 Option ['s'] ["showsubs"]     (NoArg  ShowSubs)           "balance report: show subaccounts",
 Option ['h'] ["help","usage"] (NoArg  Help)               "show this help"
 --Option ['V'] ["version"]      (NoArg  Version)            "show version"
 ]

-- | An option value from a command-line flag.
data Opt = 
    File String | 
    Begin String | 
    End String | 
    ShowSubs |
    Help |
    Version
    deriving (Show,Eq)

usage = usageInfo usagehdr options

-- | Parse the command-line arguments into ledger options, ledger command
-- name, and ledger command arguments
parseArguments :: IO ([Opt], String, [String])
parseArguments = do
  args <- getArgs
  case (getOpt RequireOrder options args) of
    (opts,[],[])       -> return (opts, defaultcmd, [])
    (opts,cmd:args,[]) -> return (opts, cmd, args)
    (_,_,errs)         -> ioError (userError (concat errs ++ usage))

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

-- | get the value of the begin date option, or a default
beginDateFromOpts :: [Opt] -> String
beginDateFromOpts opts = 
    case beginopts of
      (x:_) -> last beginopts
      _      -> defaultdate
    where
      beginopts = concatMap getbegindate opts
      getbegindate (Begin s) = [s]
      getbegindate _ = []
      defaultdate = ""

-- | get the value of the end date option, or a default
endDateFromOpts :: [Opt] -> String
endDateFromOpts opts = 
    case endopts of
      (x:_) -> last endopts
      _      -> defaultdate
    where
      endopts = concatMap getenddate opts
      getenddate (End s) = [s]
      getenddate _ = []
      defaultdate = ""

-- | ledger pattern arguments are: 0 or more account patterns
-- optionally followed by -- and 0 or more description patterns.
-- No arguments implies match all. Here we gather these into two lists.
-- parsePatternArgs :: [String] -> (Regex,Regex)
parsePatternArgs :: [String] -> ([String],[String])
parsePatternArgs args = (as, ds')
    where (as, ds) = break (=="--") args
          ds' = dropWhile (=="--") ds

-- | convert a list of strings to a regular expression matching any of them,
-- or a wildcard if there are none.
regexFor :: [String] -> Regex
regexFor [] = wildcard
regexFor ss = mkRegex $ concat $ ["("] ++ (intersperse "|" ss) ++ [")"]

wildcard :: Regex
wildcard = mkRegex ".*"

nullpats = (wildcard,wildcard)

-- testoptions RequireOrder ["foo","-v"]
-- testoptions Permute ["foo","-v"]
-- testoptions (ReturnInOrder Arg) ["foo","-v"]
-- testoptions Permute ["foo","--","-v"]
-- testoptions Permute ["-?o","--name","bar","--na=baz"]
-- testoptions Permute ["--ver","foo"]
testoptions order cmdline = putStr $ 
    case getOpt order options cmdline of
      (o,n,[]  ) -> "options=" ++ show o ++ "  args=" ++ show n
      (_,_,errs) -> concat errs ++ usage

