
module Options
where
    
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import System.Environment (getEnv)
    
import Utils

data Flag = File String | Version deriving Show
    
options :: [OptDescr Flag]
options = [
            Option ['f'] ["file"]    (OptArg inp "FILE") "ledger file, or - to read stdin"
          , Option ['v'] ["version"] (NoArg Version)     "show version number"
          ]

inp :: Maybe String -> Flag
inp  = File . fromMaybe "stdin"
    
getOptions :: [String] -> IO ([Flag], [String])
getOptions argv =
    case getOpt RequireOrder options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: hledger [OPTIONS]"

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
