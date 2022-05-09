{-|
Various additional validation checks that can be performed on a Journal.
Some are called as part of reading a file in strict mode,
others can be called only via the check command.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Read.Checks (
  journalCheckAccountsDeclared,
  journalCheckCommoditiesDeclared,
  journalCheckPayeesDeclared,
  module Hledger.Read.Checks.Ordereddates,
  module Hledger.Read.Checks.Uniqueleafnames,
)
where

import Data.Char (isSpace)
import Data.List (find)
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Safe (atMay)
import Text.Printf (printf)

import Hledger.Data
import Hledger.Read.Checks.Ordereddates
import Hledger.Read.Checks.Uniqueleafnames
import Hledger.Read.Error

-- | Check that all the journal's postings are to accounts declared with
-- account directives, returning an error message otherwise.
journalCheckAccountsDeclared :: Journal -> Either String ()
journalCheckAccountsDeclared j = mapM_ checkacct (journalPostings j)
  where
    checkacct p@Posting{paccount=a}
      | a `elem` journalAccountNamesDeclared j = Right ()
      | otherwise = Left $ 
        printf "%s:%d:%d-%d:\n%sundeclared account \"%s\"\n" f l col col2 ex a
        where
          (f,l,mcols,ex) = makePostingErrorExcerpt p finderrcols
          col  = maybe 0 fst mcols
          col2 = maybe 0 (fromMaybe 0 . snd) mcols
          finderrcols p _ _ = Just (col, Just col2)
            where
              col = 5 + if isVirtual p then 1 else 0
              col2 = col + T.length a - 1

-- | Check that all the commodities used in this journal's postings have been declared
-- by commodity directives, returning an error message otherwise.
journalCheckCommoditiesDeclared :: Journal -> Either String ()
journalCheckCommoditiesDeclared j = mapM_ checkcommodities (journalPostings j)
  where
    checkcommodities p =
      case findundeclaredcomm p of
        Nothing -> Right ()
        Just (comm, _) ->
          Left $ printf "%s:%d:%d-%d:\n%sundeclared commodity \"%s\"\n" f l col col2 ex comm
          where
            (f,l,mcols,ex) = makePostingErrorExcerpt p finderrcols
            col  = maybe 0 fst mcols
            col2 = maybe 0 (fromMaybe 0 . snd) mcols
      where
        -- Find the first undeclared commodity symbol in this posting's amount
        -- or balance assertion amount, if any. The boolean will be true if
        -- the undeclared symbol was in the posting amount.
        findundeclaredcomm :: Posting -> Maybe (CommoditySymbol, Bool)
        findundeclaredcomm Posting{pamount=amt,pbalanceassertion} =
          case (findundeclared postingcomms, findundeclared assertioncomms) of
            (Just c, _) -> Just (c, True)
            (_, Just c) -> Just (c, False)
            _           -> Nothing
          where
            postingcomms = map acommodity $ filter (not . isIgnorable) $ amountsRaw amt
              where
                -- Ignore missing amounts and zero amounts without commodity (#1767)
                isIgnorable a = (T.null (acommodity a) && amountIsZero a) || a == missingamt
            assertioncomms = [acommodity a | Just a <- [baamount <$> pbalanceassertion]]
            findundeclared = find (`M.notMember` jcommodities j)

        -- Find the best position for an error column marker when this posting
        -- is rendered by showTransaction.
        -- Reliably locating a problem commodity symbol in showTransaction output
        -- is really tricky. Some examples:
        --
        --     assets      "C $" -1 @ $ 2
        --                            ^
        --     assets      $1 = $$1
        --                      ^
        --     assets   [ANSI RED]$-1[ANSI RESET]
        --              ^
        --
        -- To simplify, we will mark the whole amount + balance assertion region, like:
        --     assets      "C $" -1 @ $ 2
        --                 ^^^^^^^^^^^^^^
        -- XXX refine this region when it's easy
        finderrcols p t txntxt =
          case transactionFindPostingIndex (==p) t of
            Nothing     -> Nothing
            Just pindex -> Just (amtstart, Just amtend)
              where
                tcommentlines = max 0 (length (T.lines $ tcomment t) - 1)
                errrelline = 1 + tcommentlines + pindex   -- XXX doesn't count posting coment lines
                errline = fromMaybe "" (T.lines txntxt `atMay` (errrelline-1))
                acctend = 4 + T.length (paccount p) + if isVirtual p then 2 else 0
                amtstart = acctend + (T.length $ T.takeWhile isSpace $ T.drop acctend errline) + 1
                amtend = amtstart + (T.length $ T.stripEnd $ T.takeWhile (/=';') $ T.drop amtstart errline)

-- | Check that all the journal's transactions have payees declared with
-- payee directives, returning an error message otherwise.
journalCheckPayeesDeclared :: Journal -> Either String ()
journalCheckPayeesDeclared j = mapM_ checkpayee (jtxns j)
  where
    checkpayee t
      | payee `elem` journalPayeesDeclared j = Right ()
      | otherwise = Left $
        printf "%s:%d:%d-%d:\n%sundeclared payee \"%s\"\n" f l col col2 ex payee
      where
        payee = transactionPayee t
        (f,l,mcols,ex) = makeTransactionErrorExcerpt t finderrcols
        col  = maybe 0 fst mcols
        col2 = maybe 0 (fromMaybe 0 . snd) mcols
        finderrcols t = Just (col, Just col2)
          where
            col = T.length (showTransactionLineFirstPart t) + 2
            col2 = col + T.length (transactionPayee t) - 1
