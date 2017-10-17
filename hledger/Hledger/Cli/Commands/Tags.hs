{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Cli.Commands.Tags (
  tagsmode
 ,tags
) 
where

import Data.List
import Data.String.Here
import qualified Data.Text as T
import Safe
import Hledger
import Hledger.Cli.CliOptions

tagsmode = hledgerCommandMode
  [here| tags
List all the tag names used in the journal. With a TAGREGEX argument,
only tag names matching the regular expression (case insensitive) are shown. 
With QUERY arguments, only transactions matching the query are considered.  
Reads the default journal file, or another specified with -f.
FLAGS
  |]
  [] -- [flagNone ["strict"] (\opts -> setboolopt "strict" opts) "makes date comparing strict"] -- 
  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "[TAGREGEX [QUERY...]]")

tags CliOpts{rawopts_=rawopts,reportopts_=ropts} j = do
  d <- getCurrentDay
  let
    args      = listofstringopt "args" rawopts
    mtagpats  = headMay args
    queryargs = drop 1 args
    q = queryFromOpts d $ ropts{query_ = unwords queryargs} 
    txns = filter (q `matchesTransaction`) $ jtxns $ journalSelectingAmountFromOpts ropts j
    tags = 
      nub $ sort $ 
      (maybe id (filter . regexMatchesCI) mtagpats) $ 
      map (T.unpack . fst) $ concatMap transactionAllTags txns
  mapM_ putStrLn tags
