{-|

The @accounts@ command lists the full names of the (query-restricted)
accounts posted to . This is similar to ledger accounts -E.

-}

module Hledger.Cli.Accounts (
  accountsmode
 ,accounts
 ,tests_Hledger_Cli_Accounts
) where

import Data.List
import System.Console.CmdArgs.Explicit as C
import Test.HUnit

import Hledger
import Prelude hiding (putStrLn)
import Hledger.Utils.UTF8IOCompat (putStrLn)
import Hledger.Cli.Options


-- | Command line options for this command.
accountsmode = (defCommandMode $ ["accounts"] ++ aliases ++ hiddenaliases) {
  modeHelp = "show names of accounts posted to" `withAliases` aliases
 ,modeGroupFlags = C.Group {
     groupUnnamed = [
      flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "with --flat, omit this many leading account name components"
     ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
  where (aliases, hiddenaliases) = (["a"],["acc"])

-- | The accounts command.
accounts :: CliOpts -> Journal -> IO ()
accounts CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  let q = queryFromOpts d ropts
  mapM_ putStrLn $ filter (q `matchesAccount`) $ nub $ sort $ map paccount $ journalPostings j

tests_Hledger_Cli_Accounts = TestList []
