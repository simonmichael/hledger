{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Cli.Commands.Tags (
  tagsmode
 ,tags
) 
where

import Data.List
import Data.String.Here
import qualified Data.Text.IO as T
import Hledger
import Hledger.Cli.CliOptions

tagsmode = hledgerCommandMode
  [here| tags
List all the tag names in use.
With a query, only matched transactions' tags are shown.
Reads the default journal file, or another specified with -f.
FLAGS
  |]
  [] -- [flagNone ["strict"] (\opts -> setboolopt "strict" opts) "makes date comparing strict"] -- 
  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "[QUERY]")

tags CliOpts{rawopts_=_rawopts,reportopts_=ropts} j = do
  d <- getCurrentDay
  let
    q = queryFromOpts d ropts
    ts = filter (q `matchesTransaction`) $ jtxns $ journalSelectingAmountFromOpts ropts j
    tags = nub $ sort $ map fst $ concatMap transactionAllTags ts
  mapM_ T.putStrLn tags
