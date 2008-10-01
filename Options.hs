module Options
where
import System.Console.GetOpt
import System.Directory
import System.Environment (getEnv)
import Data.Maybe (fromMaybe)
    
import Utils
import Types


usagehdr       = "Usage: hledger [OPTIONS] "++commands++" [ACCTPATTERNS] [-- DESCPATTERNS]\nOptions:"
commands       = "register|balance|print"
defaultcmd     = "register"

options :: [OptDescr Flag]
options = [
 Option ['f'] ["file"]     (ReqArg File "FILE") "ledger file; - means use standard input",
 Option ['s'] ["showsubs"] (NoArg ShowSubs)     "balance report: show subaccounts", -- register: show subtotals
 Option ['h'] ["help"]     (NoArg Help)         "show this help"
 --Option ['V'] ["version"]  (NoArg Version)      "show version"
 ]

data Flag = 
    File String | 
    ShowSubs |
    Help |
    Version
    deriving (Show,Eq)

parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv =
    case getOpt RequireOrder options argv of
      (opts,[],[])   -> return (opts, [defaultcmd])
      (opts,args,[]) -> return (opts, args)
      (_,_,errs)     -> ioError (userError (concat errs ++ usage))

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

usage = usageInfo usagehdr options

ledgerFilePath :: [Flag] -> IO String
ledgerFilePath = findFileFromOpts "~/ledger.dat" "LEDGER"

-- | find a file path from options, an env var or a default value
findFileFromOpts :: FilePath -> String -> [Flag] -> IO String
findFileFromOpts defaultpath envvar opts = do
  envordefault <- getEnv envvar `catch` \_ -> return defaultpath
  paths <- mapM tildeExpand $ [envordefault] ++ (concatMap getfile opts)
  return $ last paths
    where
      getfile (File s) = [s]
      getfile _ = []

tildeExpand              :: FilePath -> IO FilePath
tildeExpand ('~':[])     =  getHomeDirectory
tildeExpand ('~':'/':xs) =  getHomeDirectory >>= return . (++ ('/':xs))
-- -- ~name, requires -fvia-C or ghc 6.8
-- --import System.Posix.User
-- -- tildeExpand ('~':xs)     =  do let (user, path) = span (/= '/') xs
-- --                                pw <- getUserEntryForName user
-- --                                return (homeDirectory pw ++ path)
tildeExpand xs           =  return xs
-- -- courtesy of allberry_b

-- | ledger pattern arguments are: 0 or more account patterns
-- | optionally followed by -- and 0 or more description patterns.
-- | No arguments implies match all. We convert the arguments to
-- | a pair of regexps.
parsePatternArgs :: [String] -> (Regex,Regex)
parsePatternArgs args = (regexFor as, regexFor ds')
    where (as, ds) = break (=="--") args
          ds' = dropWhile (=="--") ds

-- | convert a list of strings to a regular expression matching any of them,
-- | or a wildcard if there are none.
regexFor :: [String] -> Regex
regexFor [] = wildcard
regexFor ss = mkRegex $ "(" ++ (unwords $ intersperse "|" ss) ++ ")"

wildcard :: Regex
wildcard = mkRegex ".*"