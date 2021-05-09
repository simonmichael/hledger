{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Hledger.Cli.Commands.Check.Uniqueleafnames (
  journalCheckUniqueleafnames
)
where

import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import Hledger
import Text.Printf (printf)

-- | Check that all the journal's postings are to accounts with a unique leaf name.
-- Otherwise, return an error message for the first offending posting.
journalCheckUniqueleafnames :: Journal -> Either String ()
journalCheckUniqueleafnames j = do
  -- find all duplicate leafnames, and the full account names they appear in
  case finddupes $ journalLeafAndFullAccountNames j of
    [] -> Right ()
    dupes ->
      -- report the first posting that references one of them (and its position), for now
      mapM_ (checkposting dupes) $ journalPostings j

finddupes :: (Ord leaf, Eq full) => [(leaf, full)] -> [(leaf, [full])]
finddupes leafandfullnames = zip dupLeafs dupAccountNames
  where dupLeafs = map (fst . head) d
        dupAccountNames = map (map snd) d
        d = dupes' leafandfullnames
        dupes' = filter ((> 1) . length)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)

journalLeafAndFullAccountNames :: Journal -> [(Text, AccountName)]
journalLeafAndFullAccountNames = map leafAndAccountName . journalAccountNamesUsed
  where leafAndAccountName a = (accountLeafName a, a)

checkposting :: [(Text,[AccountName])] -> Posting -> Either String ()
checkposting leafandfullnames Posting{paccount,ptransaction} =
  case [lf | lf@(_,fs) <- leafandfullnames, paccount `elem` fs] of
    []             -> Right ()
    (leaf,fulls):_ -> Left $ printf
      "account leaf names are not unique\nleaf name \"%s\" appears in account names: %s%s"
      leaf
      (T.intercalate ", " $ map (("\""<>).(<>"\"")) fulls)
      (case ptransaction of
        Nothing -> ""
        Just t  -> printf "\nseen in \"%s\" in transaction at: %s\n\n%s"
                    paccount
                    (showGenericSourcePos $ tsourcepos t)
                    (linesPrepend "> " . (<>"\n") . textChomp $ showTransaction t) :: String)
