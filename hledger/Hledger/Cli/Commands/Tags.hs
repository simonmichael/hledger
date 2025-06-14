{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Hledger.Cli.Commands.Tags (
  tagsmode
 ,tags
)
where

import qualified Control.Monad.Fail as Fail
import Data.List.Extra (nubSort)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Safe
import System.Console.CmdArgs.Explicit

import Hledger
import Hledger.Cli.CliOptions


tagsmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Tags.txt")
  [
   flagNone ["used"]         (setboolopt "used")       "list tags used"
  ,flagNone ["declared"]     (setboolopt "declared")   "list tags declared"
  ,flagNone ["undeclared"]   (setboolopt "undeclared") "list tags used but not declared"
  ,flagNone ["unused"]       (setboolopt "unused")     "list tags declared but not used"
  ,flagNone ["values"]       (setboolopt "values")     "list tag values instead of tag names"
  ,flagNone ["parsed"]       (setboolopt "parsed")     "show them in the order they were parsed (mostly), including duplicates"
  ]
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[TAGREGEX [QUERY..]]")

tags :: CliOpts -> Journal -> IO ()
tags opts@CliOpts{rawopts_=rawopts, reportspec_=rspec@ReportSpec{_rsQuery=_q, _rsReportOpts=ropts}} j = do
  let today = _rsDay rspec
      args = listofstringopt "args" rawopts
  -- For convenience/power, the first argument is a tag name regex, 
  -- separate from the main query arguments: hledger tags [TAGREGEX [QUERYARGS..]]
  -- So we have to re-parse the query here. Overcomplicated ?
  mtagpat <- mapM (either Fail.fail pure . toRegexCI . T.pack) $ headMay args
  let
    values   = boolopt "values" rawopts
    parsed   = boolopt "parsed" rawopts
    empty    = empty_ ropts
    querystr = map T.pack $ drop 1 args
  query <- either usageError (return . fst) $ parseQueryList today querystr
  let
    q = simplifyQuery $ And [queryFromFlags ropts, query]
    txns = filter (q `matchesTransaction`) $ jtxns $ journalApplyValuationFromOpts rspec j
    accts =
      -- also search for tags in matched account declarations,
      -- unless there is a query for something transaction-specific, like date: or amt:.
      if dbg5 "queryIsTransactionRelated" $ queryIsTransactionRelated $ dbg4 "q" q
      then []
      else filter (matchesAccountExtra (journalAccountType j) (journalInheritedAccountTags j) q) $
           map fst $ jdeclaredaccounts j

    used       = dbg5 "used"       $ concatMap (journalAccountTags j) accts ++ concatMap transactionAllTags txns
    declared'  = dbg5 "declared"   $ filter (q `matchesTag`) $ map (,"") $ journalTagsDeclared j
    (usednames, declarednames) = (map fst used, map fst declared')
    unused     = dbg5 "unused"     $ filter (not . (`elem` usednames) . fst) declared'
    undeclared = dbg5 "undeclared" $ filter (not . (`elem` declarednames) . fst) used
    all'       = dbg5 "all"        $ declared' <> used

    tags' =
      case usedOrDeclaredFromOpts opts of
        Nothing         -> all'
        Just Used       -> used
        Just Declared   -> declared'
        Just Undeclared -> undeclared
        Just Unused     -> unused

    results =
      (if parsed then id else nubSort)
      [ r
      | (t,v) <- tags'
      , maybe True (`regexMatchText` t) mtagpat
      , let r = if values then v else t
      , not (values && T.null v && not empty)
      ]

  mapM_ T.putStrLn results
