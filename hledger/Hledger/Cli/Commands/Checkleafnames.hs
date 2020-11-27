{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Checkleafnames (
  checkleafnamesmode
 ,checkleafnames
)
where

import Data.Function
import Data.List
import Data.List.Extra (nubSort)
import qualified Data.Text as T
import Hledger
import Hledger.Cli.CliOptions
import System.Console.CmdArgs.Explicit
import Text.Printf

checkleafnamesmode :: Mode RawOpts
checkleafnamesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Checkleafnames.txt")
  []
  [generalflagsgroup1]
  hiddenflags
  ([], Nothing)

checkleafnames _opts j = mapM_ render $ checkleafnames' $ accountsNames j

accountsNames :: Journal -> [(String, AccountName)]
accountsNames j = map leafAndAccountName as
  where leafAndAccountName a = (T.unpack $ accountLeafName a, a)
        ps = journalPostings j
        as = nubSort $ map paccount ps

checkleafnames' :: (Ord k, Eq k) => [(k, v)] -> [(k, [v])]
checkleafnames' l = zip dupLeafs dupAccountNames
  where dupLeafs = map (fst . head) d
        dupAccountNames = map (map snd) d
        d = dupes' l
        dupes' = filter ((> 1) . length)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)

render :: (String, [AccountName]) -> IO ()
render (leafName, accountNameL) = printf "%s as %s\n" leafName (intercalate ", " (map T.unpack accountNameL))
