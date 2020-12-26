{-# LANGUAGE OverloadedStrings #-}

module Hledger.Cli.Commands.Check.Uniqueleafnames (
  journalCheckUniqueleafnames
)
where

import Data.Function
import Data.List
import Data.List.Extra (nubSort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger

journalCheckUniqueleafnames :: Journal -> Either String ()
journalCheckUniqueleafnames j = do
  let dupes = checkdupes' $ accountsNames j
  if null dupes
  then Right ()
  else Left . T.unpack $
    -- XXX make output more like Checkdates.hs, Check.hs etc.
    foldMap render dupes
    where
      render (leafName, accountNameL) =
        leafName <> " as " <> T.intercalate ", " accountNameL

checkdupes' :: (Ord k, Eq k) => [(k, v)] -> [(k, [v])]
checkdupes' l = zip dupLeafs dupAccountNames
  where dupLeafs = map (fst . head) d
        dupAccountNames = map (map snd) d
        d = dupes' l
        dupes' = filter ((> 1) . length)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)

accountsNames :: Journal -> [(Text, AccountName)]
accountsNames j = map leafAndAccountName as
  where leafAndAccountName a = (accountLeafName a, a)
        ps = journalPostings j
        as = nubSort $ map paccount ps
