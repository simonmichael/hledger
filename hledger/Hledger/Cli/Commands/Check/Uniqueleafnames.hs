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
import Data.Maybe (fromMaybe)

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
checkposting leafandfullnames p@Posting{paccount=a} =
  case [lf | lf@(_,fs) <- leafandfullnames, a `elem` fs] of
    []             -> Right ()
    (leaf,fulls):_ -> Left $ printf
      "%s:%d:%d-%d:\n%saccount leaf name \"%s\" is not unique\nit is used in account names: %s" 
      f l col col2 ex leaf accts
      where
        -- t = fromMaybe nulltransaction ptransaction  -- XXX sloppy
        col  = maybe 0 fst mcols
        col2 = maybe 0 (fromMaybe 0 . snd) mcols
        (f,l,mcols,ex) = makePostingErrorExcerpt p finderrcols
          where
            finderrcols p _ _ = Just (col, Just col2)
              where
                alen = T.length $ paccount p
                llen = T.length $ accountLeafName a
                col = 5 + (if isVirtual p then 1 else 0) + alen - llen
                col2 = col + llen - 1
        accts = T.intercalate ", " $ map (("\""<>).(<>"\"")) fulls
