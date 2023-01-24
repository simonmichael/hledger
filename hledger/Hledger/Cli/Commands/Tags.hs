{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import System.Console.CmdArgs.Explicit as C
import Hledger
import Hledger.Cli.CliOptions

tagsmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Tags.txt")
  [flagNone ["values"] (setboolopt "values") "list tag values instead of tag names"
  ,flagNone ["parsed"] (setboolopt "parsed") "show tags/values in the order they were parsed, including duplicates"
  ]
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[TAGREGEX [QUERY...]]")

tags :: CliOpts -> Journal -> IO ()
tags CliOpts{rawopts_=rawopts,reportspec_=rspec} j = do
  let today = _rsDay rspec
      args = listofstringopt "args" rawopts
  -- first argument is a tag name pattern, others are a hledger query: hledger tags [TAGREGEX [QUERYARGS..]]
  mtagpat <- mapM (either Fail.fail pure . toRegexCI . T.pack) $ headMay args
  let
    querystr = map T.pack $ drop 1 args
    values   = boolopt "values" rawopts
    parsed   = boolopt "parsed" rawopts
    empty    = empty_ $ _rsReportOpts rspec
  query <- either usageError (return . fst) $ parseQueryList today querystr
  let
    q = simplifyQuery $ And [queryFromFlags $ _rsReportOpts rspec, query]
    matchedtxns = filter (q `matchesTransaction`) $ jtxns $ journalApplyValuationFromOpts rspec j
    -- also list tags from matched account declarations, but not if there is
    -- a query for something transaction-related, like date: or amt:.
    matchedaccts = dbg4 "accts" $
      if dbg4 "queryIsTransactionRelated" $ queryIsTransactionRelated $ dbg4 "q" q
      then []
      else filter (matchesAccountExtra (journalAccountType j) (journalInheritedAccountTags j) q) $
           map fst $ jdeclaredaccounts j
    tagsorvalues =
      (if parsed then id else nubSort)
      [ r
      | (t,v) <- concatMap (journalAccountTags j) matchedaccts ++ concatMap transactionAllTags matchedtxns
      , maybe True (`regexMatchText` t) mtagpat
      , let r = if values then v else t
      , not (values && T.null v && not empty)
      ]
  mapM_ T.putStrLn tagsorvalues
