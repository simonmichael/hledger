
module Options (module Options, usageInfo)
where
import System.Console.GetOpt
import System.Environment (getEnv)
import Data.Maybe (fromMaybe)
    
import Utils


usageHeader = "Usage: hledger [OPTIONS] register|balance [MATCHARGS]"

getOptions :: [String] -> IO ([Flag], [String])
getOptions argv =
    case getOpt RequireOrder options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageHeader options))

options :: [OptDescr Flag]
options = [
            Option ['v'] ["version"] (NoArg Version)     "show version number"
          , Option ['f'] ["file"]    (OptArg readFileOpt "FILE") "ledger file, or - to read stdin"
          , Option ['s'] ["subtotal"] (NoArg ShowSubs)     "balance: show sub-accounts" --; register: show subtotals"
          ]

data Flag = Version | File String | ShowSubs deriving (Show,Eq)
    
readFileOpt :: Maybe String -> Flag
readFileOpt  = File . fromMaybe "stdin"
    
getFile :: Flag -> String
getFile (File s) = s
getFile _ = []

getLedgerFilePath :: [Flag] -> IO String
getLedgerFilePath opts = do
  defaultpath <- tildeExpand "~/ledger.dat"
  envordefault <- getEnv "LEDGER" `catch` \_ -> return defaultpath
  return $ last $ [envordefault] ++ (filter (/= "") (map getFile opts))

-- ledger pattern args are a list of account patterns optionally followed
-- by -- and a list of description patterns
ledgerPatternArgs :: [String] -> ([String],[String])
ledgerPatternArgs args = 
    case "--" `elem` args of
      True -> ((takeWhile (/= "--") args), tail $ (dropWhile (/= "--") args))
      False -> (args,[])

getDepth :: [Flag] -> Int
getDepth opts = 
    maximum $ [1] ++ map depthval opts where
        depthval (ShowSubs) = 9999
        depthval _ = 1
