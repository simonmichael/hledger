#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package here
   --package safe
   --package text
-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}
{-# LANGUAGE QuasiQuotes #-}

import Hledger
import Hledger.Cli
import Text.Printf (printf)
-- import System.Environment (getArgs)
import Data.List
import Data.Function
import Data.String.Here
import qualified Data.Text as T

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  [here| dupes
Reports duplicates in the account tree: account names having the same leaf
but different prefixes. In other words, two or more leaves that are
categorized differently.
Reads the default journal file, or another specified as an argument.
 
http://stefanorodighiero.net/software/hledger-dupes.html
  |]
  []
  [generalflagsgroup1]
  []
  ([], Nothing)
------------------------------------------------------------------------------

main = do
  opts <- getHledgerCliOpts cmdmode
  withJournalDo opts $ \CliOpts{rawopts_=_opts,reportopts_=_ropts} j -> do
    mapM_ render $ dupes $ accountsNames j

accountsNames :: Journal -> [(String, AccountName)]
accountsNames j = map leafAndAccountName as
  where leafAndAccountName a = (T.unpack $ accountLeafName a, a)
        ps = journalPostings j
        as = nub $ sort $ map paccount ps


dupes :: (Ord k, Eq k) => [(k, v)] -> [(k, [v])]
dupes l = zip dupLeafs dupAccountNames
  where dupLeafs = map (fst . head) d
        dupAccountNames = map (map snd) d
        d = dupes' l
        dupes' = filter ((> 1) . length)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)

render :: (String, [AccountName]) -> IO ()
render (leafName, accountNameL) = printf "%s as %s\n" leafName (concat $ intersperse ", " (map T.unpack accountNameL))
