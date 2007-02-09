module Options where
    
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import System.Environment (getEnv)
--import TildeExpand -- confuses my ghc 6.7
    
data Flag = File String | Version deriving Show
    
options :: [OptDescr Flag]
options = [
           Option ['f']     ["file"] (OptArg inp "FILE") "ledger file, or - to read stdin"
          , Option ['v'] ["version"] (NoArg Version) "show version number"
          ]

inp :: Maybe String -> Flag
inp  = File . fromMaybe "stdin"
    
getOptions :: [String] -> IO ([Flag], [String])
getOptions argv =
    case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: hledger [OPTIONS]"

get_content :: Flag -> Maybe String
get_content (File s) = Just s

--defaultLedgerFile = tildeExpand "~/ledger.dat"
defaultLedgerFile = "ledger.dat"

ledgerFilePath :: IO String
ledgerFilePath = do
  getEnv "LEDGER" `catch` \_ -> return defaultLedgerFile >>= return
