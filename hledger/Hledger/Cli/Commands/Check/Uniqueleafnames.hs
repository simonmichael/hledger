module Hledger.Cli.Commands.Check.Uniqueleafnames (
  journalCheckUniqueleafnames
)
where

import Data.Function
import Data.List
import Data.List.Extra (nubSort)
import qualified Data.Text as T
import Hledger
import Text.Printf

journalCheckUniqueleafnames :: Journal -> Either String ()
journalCheckUniqueleafnames j = do
  let dupes = checkdupes' $ accountsNames j
  if null dupes
  then Right ()
  else Left $ 
    -- XXX make output more like Checkdates.hs, Check.hs etc.
    concatMap render dupes
    where
      render (leafName, accountNameL) = 
        printf "%s as %s\n" leafName (intercalate ", " (map T.unpack accountNameL))

checkdupes' :: (Ord k, Eq k) => [(k, v)] -> [(k, [v])]
checkdupes' l = zip dupLeafs dupAccountNames
  where dupLeafs = map (fst . head) d
        dupAccountNames = map (map snd) d
        d = dupes' l
        dupes' = filter ((> 1) . length)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)

accountsNames :: Journal -> [(String, AccountName)]
accountsNames j = map leafAndAccountName as
  where leafAndAccountName a = (T.unpack $ accountLeafName a, a)
        ps = journalPostings j
        as = nubSort $ map paccount ps
