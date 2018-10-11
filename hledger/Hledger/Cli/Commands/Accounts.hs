{-|

The @accounts@ command lists account names:

- in flat mode (default), it lists the full names of accounts posted to by matched postings,
  clipped to the specified depth, possibly with leading components dropped.

- in tree mode, it shows the indented short names of accounts posted to by matched postings,
  and their parents, to the specified depth.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Hledger.Cli.Commands.Accounts (
  accountsmode
 ,accounts
) where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.CmdArgs.Explicit as C

import Hledger
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
     ,flagNone ["codes"] (\opts -> setboolopt "codes" opts) "also show numeric account codes"
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

  -- 1. identify the accounts we'll show
  d <- getCurrentDay
  let tree     = tree_ ropts
      declared = boolopt "declared" rawopts
      used     = boolopt "used"     rawopts
      q        = queryFromOpts d ropts
      -- a depth limit will clip and exclude account names later, but we don't want to exclude accounts at this stage
      nodepthq = dbg1 "nodepthq" $ filterQuery (not . queryIsDepth) q
      -- just the acct: part of the query will be reapplied later, after clipping
      acctq    = dbg1 "acctq" $ filterQuery queryIsAcct q
      depth    = dbg1 "depth" $ queryDepth $ filterQuery queryIsDepth q
      matcheddeclaredaccts = dbg1 "matcheddeclaredaccts" $ filter (matchesAccount nodepthq) $ jdeclaredaccounts j
      matchedusedaccts     = dbg5 "matchedusedaccts" $ map paccount $ journalPostings $ filterJournalPostings nodepthq j
      accts                = dbg5 "accts to show" $ -- no need to nub/sort, accountTree will
        if | declared     && not used -> matcheddeclaredaccts
           | not declared && used     -> matchedusedaccts
           | otherwise                -> matcheddeclaredaccts ++ matchedusedaccts 

  -- 2. sort them by declaration order and name, at each level of their tree structure
      sortedaccts = sortAccountNamesByDeclaration j tree accts

  -- 3. if there's a depth limit, depth-clip and remove any no longer useful items 
      clippedaccts =
        dbg1 "clippedaccts" $
        filter (matchesAccount acctq) $  -- clipping can leave accounts that no longer match the query, remove such
        nub $                          -- clipping can leave duplicates (adjacent, hopefully)
        filter (not . T.null) $        -- depth:0 can leave nulls
        map (clipAccountName depth) $  -- clip at depth if specified 
        sortedaccts 

  -- 4. print what remains as a list or tree, maybe applying --drop in the former case 
  mapM_ (T.putStrLn . render) clippedaccts
    where
      render a 
        | tree_ ropts = T.replicate (2 * (accountNameLevel a - 1)) " " <> accountLeafName a
        | otherwise   = maybeAccountNameDrop ropts a

