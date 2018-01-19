{-|

The @accounts@ command lists account names:

- in flat mode (default), it lists the full names of accounts posted to by matched postings,
  clipped to the specified depth, possibly with leading components dropped.

- in tree mode, it shows the indented short names of accounts posted to by matched postings,
  and their parents, to the specified depth.

-}

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Cli.Commands.Accounts (
  accountsmode
 ,accounts
 ,tests_Hledger_Cli_Commands_Accounts
) where

import Data.List
import Data.Monoid
-- import Data.Text (Text)
import qualified Data.Text as T
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
     "This command lists account names, either declared with account directives"
    ,"(--declared), posted to (--used), or both (default)."
    ,"With query arguments, only matched account names and account names" 
    ,"referenced by matched postings are shown."
    ,"It shows a flat list by default. With `--tree`, it uses indentation to"
    ,"show the account hierarchy."
    ,"In flat mode you can add `--drop N` to omit the first few account name components."
    ,"Account names can be depth-clipped with `--depth N` or depth:N."
   ]
 ,modeGroupFlags = C.Group {
     groupUnnamed = [
      flagNone ["declared"] (\opts -> setboolopt "declared" opts) "show account names declared with account directives"
     ,flagNone ["used"] (\opts -> setboolopt "used" opts) "show account names referenced by transactions"
     ,flagNone ["tree"] (\opts -> setboolopt "tree" opts) "show short account names, as a tree"
     ,flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show full account names, as a list (default)"
     ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
     ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
  where aliases = ["a"]

-- | The accounts command.
accounts :: CliOpts -> Journal -> IO ()
accounts CliOpts{rawopts_=rawopts, reportopts_=ropts} j = do
  d <- getCurrentDay
  let q = queryFromOpts d ropts
      nodepthq = dbg1 "nodepthq" $ filterQuery (not . queryIsDepth) q
      depth    = dbg1 "depth" $ queryDepth $ filterQuery queryIsDepth q
      matcheddeclaredaccts = dbg1 "matcheddeclaredaccts" $ nub $ sort $ filter (matchesAccount q) $ jaccounts j
      matchedps = dbg1 "ps" $ journalPostings $ filterJournalPostings nodepthq j
      matchedusedaccts = dbg1 "matchedusedaccts" $ nub $ sort $ filter (not . T.null) $ map (clipAccountName depth) $ map paccount matchedps
      used     = boolopt "used"   rawopts
      declared = boolopt "declared" rawopts
      as | declared     && not used = matcheddeclaredaccts
         | not declared && used     = matchedusedaccts
         | otherwise                = nub $ sort $ matcheddeclaredaccts ++ matchedusedaccts
      as' | tree_ ropts = expandAccountNames as
          | otherwise   = as
      render a | tree_ ropts = T.replicate (2 * (accountNameLevel a - 1)) " " <> accountLeafName a
               | otherwise   = maybeAccountNameDrop ropts a
  mapM_ (putStrLn . T.unpack . render) as'

tests_Hledger_Cli_Commands_Accounts = TestList []
