{-# LANGUAGE QuasiQuotes #-}

module Hledger.Cli.Commands.Checkdupes (
  checkdupesmode
 ,checkdupes
) 
where

import Data.Function
import Data.List
import Data.String.Here
import qualified Data.Text as T
import Hledger
import Hledger.Cli.CliOptions
import System.Console.CmdArgs.Explicit
import Text.Printf

checkdupesmode :: Mode RawOpts
checkdupesmode = hledgerCommandMode
  [here| check-dupes
Reports account names having the same leaf but different prefixes. 
In other words, two or more leaves that are categorized differently.
Reads the default journal file, or another specified as an argument.
An example: http://stefanorodighiero.net/software/hledger-dupes.html
  |]
  []
  [generalflagsgroup1]
  []
  ([], Nothing)

checkdupes _opts j = mapM_ render $ checkdupes' $ accountsNames j

accountsNames :: Journal -> [(String, AccountName)]
accountsNames j = map leafAndAccountName as
  where leafAndAccountName a = (T.unpack $ accountLeafName a, a)
        ps = journalPostings j
        as = nub $ sort $ map paccount ps

checkdupes' :: (Ord k, Eq k) => [(k, v)] -> [(k, [v])]
checkdupes' l = zip dupLeafs dupAccountNames
  where dupLeafs = map (fst . head) d
        dupAccountNames = map (map snd) d
        d = dupes' l
        dupes' = filter ((> 1) . length)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)

render :: (String, [AccountName]) -> IO ()
render (leafName, accountNameL) = printf "%s as %s\n" leafName (concat $ intersperse ", " (map T.unpack accountNameL))
