{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Tags (
  tagsmode
 ,tags
)
where

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

tags CliOpts{rawopts_=rawopts,reportopts_=ropts} j = do
  d <- getCurrentDay
  let
    args      = listofstringopt "args" rawopts
    mtagpat   = headMay args
    queryargs = drop 1 args
    values    = boolopt "values" rawopts
    parsed    = boolopt "parsed" rawopts
    empty     = empty_ ropts
    q = queryFromOpts d $ ropts{query_ = unwords $ map quoteIfNeeded queryargs}
    txns = filter (q `matchesTransaction`) $ jtxns $ journalSelectingAmountFromOpts ropts j
    tagsorvalues =
      (if parsed then id else nubSort)
      [ r
      | (t,v) <- concatMap transactionAllTags txns
      , maybe True (`regexMatchesCI` T.unpack t) mtagpat
      , let r = if values then v else t
      , not (values && T.null v && not empty)
      ]
  mapM_ T.putStrLn tagsorvalues
