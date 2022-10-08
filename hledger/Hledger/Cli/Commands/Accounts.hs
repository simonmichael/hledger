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
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Accounts (
  accountsmode
 ,accounts
) where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.CmdArgs.Explicit as C

import Hledger
import Hledger.Cli.CliOptions
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Safe (headDef)


-- | Command line options for this command.
accountsmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Accounts.txt")
  (flattreeflags False ++
  [flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
  ,flagNone ["used","u"] (setboolopt "used") "show accounts used by transactions"
  ,flagNone ["declared","d"] (setboolopt "declared") "show accounts declared by account directive"
  ,flagNone ["unused"] (setboolopt "unused") "show only accounts declared but not used"
  ,flagNone ["undeclared"] (setboolopt "undeclared") "show only accounts used but not declared"
  ,flagNone ["find"] (setboolopt "find") "find the first account matched by the first command argument (a case-insensitive infix regexp or account name)"
  ,flagNone ["types"] (setboolopt "types") "also show account types when known"
  ,flagNone ["positions"] (setboolopt "positions") "also show where accounts were declared"
  ,flagNone ["directives"] (setboolopt "directives") "show as account directives, for use in journals"
  ])
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | The accounts command.
accounts :: CliOpts -> Journal -> IO ()
accounts CliOpts{rawopts_=rawopts, reportspec_=ReportSpec{_rsQuery=query,_rsReportOpts=ropts}} j = do

  -- 1. identify the accounts we'll show
  let tree     = tree_ ropts
      used = boolopt "used"     rawopts
      decl = boolopt "declared" rawopts
      unused = boolopt "unused" rawopts
      undecl = boolopt "undeclared" rawopts
      find_ = boolopt "find" rawopts
      types = boolopt "types"    rawopts
      positions = boolopt "positions" rawopts
      directives = boolopt "directives" rawopts
      -- a depth limit will clip and exclude account names later, but we don't want to exclude accounts at this stage
      nodepthq = dbg4 "nodepthq" $ filterQuery (not . queryIsDepth) query
      -- just the acct: part of the query will be reapplied later, after clipping
      acctq = dbg4 "acctq" $ filterQuery queryIsAcct query
      dep = dbg4 "depth" $ queryDepth $ filterQuery queryIsDepth query
      matcheddeclaredaccts = dbg5 "matcheddeclaredaccts" $
        nub $
        filter (matchesAccountExtra (journalAccountType j) (journalInheritedAccountTags j) nodepthq) $
        map fst $ jdeclaredaccounts j
      matchedusedaccts = dbg5 "matchedusedaccts" $ nub $ map paccount $ journalPostings $ filterJournalPostings nodepthq j
      matchedunusedaccts = dbg5 "matchedunusedaccts" $ nub $ matcheddeclaredaccts \\ matchedusedaccts
      matchedundeclaredaccts = dbg5 "matchedundeclaredaccts" $ nub $ matchedusedaccts \\ matcheddeclaredaccts
      -- keep synced with aregister
      matchedacct = dbg5 "matchedacct" $
        fromMaybe (error' $ show apat ++ " did not match any account.")   -- PARTIAL:
            . firstMatch $ journalAccountNamesDeclaredOrImplied j
        where
          firstMatch = case toRegexCI $ T.pack apat of
              Right re -> find (regexMatchText re)
              Left  _  -> const Nothing
          apat = headDef
            (error' "With --find, please provide an account name or\naccount pattern (case-insensitive, infix, regexp) as first command argument.")
            $ listofstringopt "args" rawopts

      accts = dbg5 "accts to show" $ if
        | not decl && used     -> matchedusedaccts
        | decl     && not used -> matcheddeclaredaccts
        | unused               -> matchedunusedaccts
        | undecl               -> matchedundeclaredaccts
        | find_                -> [matchedacct]
        | otherwise            -> matcheddeclaredaccts ++ matchedusedaccts

  -- 2. sort them by declaration order (then undeclared accounts alphabetically)
  -- within each group of siblings
      sortedaccts = sortAccountNamesByDeclaration j tree accts

  -- 3. if there's a depth limit, depth-clip and remove any no longer useful items
      clippedaccts =
        dbg4 "clippedaccts" $
        filter (matchesAccount acctq) $  -- clipping can leave accounts that no longer match the query, remove such
        nub $                            -- clipping can leave duplicates (adjacent, hopefully)
        filter (not . T.null) $          -- depth:0 can leave nulls
        map (clipAccountName dep) $      -- clip at depth if specified
        sortedaccts

  -- 4. print what remains as a list or tree, maybe applying --drop in the former case.
  -- Add various bits of info if enabled.
  let
    showKeyword = if directives then "account " else ""
    -- some contortions here to show types nicely aligned
    showName a = case accountlistmode_ ropts of
      ALTree -> indent <> accountLeafName droppedName
      ALFlat -> droppedName
      where
        indent      = T.replicate (2 * (max 0 (accountNameLevel a - drop_ ropts) - 1)) " "
        droppedName = accountNameDrop (drop_ ropts) a
    showType a 
      | types     = pad a <> "    ; type: " <> maybe "" (T.pack . show) (journalAccountType j a)
      | otherwise = ""
    showAcctDeclOrder a
      | positions =
        (if types then "," else pad a <> "    ;") <>
        case lookup a $ jdeclaredaccounts j of
          Just adi ->
            " declared at " <> (T.pack $ sourcePosPretty $ adisourcepos adi) <>  -- TODO: hide the column number
            ", overall declaration order " <> (T.pack $ show $ adideclarationorder adi)
          Nothing -> " undeclared"
      | otherwise = ""
    pad a = T.replicate (maxwidth - T.length (showName a)) " "
    maxwidth = maximum $ map (T.length . showName) clippedaccts

  forM_ clippedaccts $ \a -> T.putStrLn $ showKeyword <> showName a <> showType a <> showAcctDeclOrder a
