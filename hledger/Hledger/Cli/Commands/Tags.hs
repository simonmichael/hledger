{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Tags (
  tagsmode
 ,tags
) 
where

import Data.List
import qualified Data.Text as T
import Safe
import Hledger
import Hledger.Cli.CliOptions

tagsmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Tags.txt")
  [] -- [flagNone ["strict"] (setboolopt "strict") "makes date comparing strict"] -- 
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
