
module Options (module Options, usageInfo)
where
import System.Console.GetOpt
import System.Environment (getEnv)
import Data.Maybe (fromMaybe)
    
import Utils


data Flag = Version | File String | ShowSubs
            deriving (Show,Eq)
    
options :: [OptDescr Flag]
options = [
            Option ['v'] ["version"] (NoArg Version)     "show version number"
          , Option ['f'] ["file"]    (OptArg inp "FILE") "ledger file, or - to read stdin"
          , Option ['s'] ["subtotal"] (NoArg ShowSubs)     "balance: show sub-accounts" --; register: show subtotals"
          ]

inp :: Maybe String -> Flag
inp  = File . fromMaybe "stdin"
    
getOptions :: [String] -> IO ([Flag], [String])
getOptions argv =
    case getOpt RequireOrder options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageHeader options))

usageHeader = "Usage: hledger [OPTIONS] register|balance [MATCHARGS]"

get_content :: Flag -> Maybe String
get_content (File s) = Just s

defaultLedgerFile = "~/ledger.dat"

getLedgerFilePath :: IO String
getLedgerFilePath = do
  defaultpath <- tildeExpand defaultLedgerFile
  getEnv "LEDGER" `catch` \_ -> return defaultpath >>= return

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
