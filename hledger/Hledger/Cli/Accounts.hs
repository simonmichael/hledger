{-|

The @accounts@ command lists account names:

- in flat mode (default), it lists the full names of accounts posted to by matched postings,
  clipped to the specified depth, possibly with leading components dropped.

- in tree mode, it shows the indented short names of accounts posted to by matched postings,
  and their parents, to the specified depth.

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
import Hledger.Cli.CliOptions


-- | Command line options for this command.
accountsmode = (defCommandMode $ ["accounts"] ++ aliases) {
  modeHelp = "show account names" `withAliases` aliases
 ,modeHelpSuffix = [
    "This command lists the accounts referenced by matched postings (and in tree mode, their parents as well). The accounts can be depth-clipped (--depth N) or have their leading parts trimmed (--drop N)."
   ]
 ,modeGroupFlags = C.Group {
     groupUnnamed = [
      flagNone ["tree"] (\opts -> setboolopt "tree" opts) "show short account names, as a tree"
     ,flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show full account names, as a list (default)"
     ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
     ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
  where aliases = []

-- | The accounts command.
accounts :: CliOpts -> Journal -> IO ()
accounts CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  let q = queryFromOpts d ropts
      nodepthq = dbg1 "nodepthq" $ filterQuery (not . queryIsDepth) q
      depth    = dbg1 "depth" $ queryDepth $ filterQuery queryIsDepth q
      ps = dbg1 "ps" $ journalPostings $ filterJournalPostings nodepthq j
      as = dbg1 "as" $ nub $ filter (not . null) $ map (clipAccountName depth) $ sort $ map paccount ps
      as' | tree_ ropts = expandAccountNames as
          | otherwise   = as
      render a | tree_ ropts = replicate (2 * (accountNameLevel a - 1)) ' ' ++ accountLeafName a
               | otherwise   = maybeAccountNameDrop ropts a
  mapM_ (putStrLn . render) as'

tests_Hledger_Cli_Accounts = TestList []
