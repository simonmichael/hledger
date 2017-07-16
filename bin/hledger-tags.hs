#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package hledger
   --package here
   --package text
-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.List
import Data.String.Here
import qualified Data.Text.IO as T
import Hledger
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
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
------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- getHledgerCliOpts cmdmode
  withJournalDo opts $
   \CliOpts{rawopts_=_rawopts,reportopts_=ropts} j -> do
    d <- getCurrentDay
    let
      q = queryFromOpts d ropts
      ts = filter (q `matchesTransaction`) $ jtxns $ journalSelectingAmountFromOpts ropts j
      tags = nub $ sort $ map fst $ concatMap transactionAllTags ts
    mapM_ T.putStrLn tags
