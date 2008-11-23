module Options 
where
import System
import System.Console.GetOpt
import System.Directory
import Text.Printf
import Ledger.AccountName (negativepatternchar)

usagehdr    = "Usage: hledger [OPTS] balance|print|register [ACCTPATS] [-- DESCPATS]\n\nOptions"++warning++":"
warning     = if negativepatternchar=='-' then " (must appear before command)" else " (can appear anywhere)"
usageftr    = "\n\
              \Commands (may be abbreviated):\n\
              \balance  - show account balances\n\
              \print    - show parsed and reformatted ledger entries\n\
              \register - show register transactions\n\
              \\n\
              \Account and description patterns are regular expressions, optionally prefixed\n\
              \with " ++ [negativepatternchar] ++ " to make them negative.\n"
defaultfile = "~/.ledger"
fileenvvar  = "LEDGER"
optionorder = if negativepatternchar=='-' then RequireOrder else Permute

-- | Command-line options we accept.
options :: [OptDescr Opt]
options = [
 Option ['f'] ["file"]         (ReqArg File "FILE")        "ledger file; - means use standard input",
 Option ['b'] ["begin"]        (ReqArg Begin "YYYY/MM/DD") "report on entries on or after this date",
 Option ['e'] ["end"]          (ReqArg End "YYYY/MM/DD")   "report on entries prior to this date",
 Option ['C'] ["cleared"]      (NoArg  Cleared)            "report only on cleared entries",
 Option ['R'] ["real"]         (NoArg  Real)               "report only on real (non-virtual) transactions",
 Option ['s'] ["showsubs"]     (NoArg  ShowSubs)           "in the balance report, include subaccounts",
 Option ['h'] ["help","usage"] (NoArg  Help)               "show this help",
 Option ['V'] ["version"]      (NoArg  Version)            "show version"
 ]

-- | An option value from a command-line flag.
data Opt = 
    File String | 
    Begin String | 
    End String | 
    Cleared | 
    Real | 
    ShowSubs |
    Help |
    Version
    deriving (Show,Eq)

usage = usageInfo usagehdr options ++ usageftr

versionno = "0.2"
version = printf "hledger version %s \n" versionno :: String

-- | Parse the command-line arguments into ledger options, ledger command
-- name, and ledger command arguments
parseArguments :: IO ([Opt], String, [String])
parseArguments = do
  args <- getArgs
  case (getOpt optionorder options args) of
    (opts,cmd:args,[]) -> return (opts, cmd, args)
    (opts,[],[])       -> return (opts, [], [])
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

-- | Gather any ledger-style account/description pattern arguments into
-- two lists.  These are 0 or more account patterns optionally followed by
-- -- and 0 or more description patterns.
parseAccountDescriptionArgs :: [String] -> ([String],[String])
parseAccountDescriptionArgs args = (as, ds')
    where (as, ds) = break (=="--") args
          ds' = dropWhile (=="--") ds

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

