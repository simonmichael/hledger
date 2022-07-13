{-|
Various additional validation checks that can be performed on a Journal.
Some are called as part of reading a file in strict mode,
others can be called only via the check command.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Data.JournalChecks (
  journalCheckAccounts,
  journalCheckCommodities,
  journalCheckPayees,
  module Hledger.Data.JournalChecks.Ordereddates,
  module Hledger.Data.JournalChecks.Uniqueleafnames,
)
where

import Data.Char (isSpace)
import Data.List (find)
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Safe (atMay)
import Text.Printf (printf)

import Hledger.Data.Errors
import Hledger.Data.Journal
import Hledger.Data.JournalChecks.Ordereddates
import Hledger.Data.JournalChecks.Uniqueleafnames
import Hledger.Data.Posting (isVirtual)
import Hledger.Data.Types
import Hledger.Data.Amount (amountIsZero, amountsRaw, missingamt)
import Hledger.Data.Transaction (transactionPayee, showTransactionLineFirstPart)

-- | Check that all the journal's postings are to accounts  with
-- account directives, returning an error message otherwise.
journalCheckAccounts :: Journal -> Either String ()
journalCheckAccounts j = mapM_ checkacct (journalPostings j)
  where
    checkacct p@Posting{paccount=a}
      | a `elem` journalAccountNamesDeclared j = Right ()
      | otherwise = Left $ printf (unlines [
           "%s:%d:"
          ,"%s"
          ,"Strict account checking is enabled, and"
          ,"account %s has not been declared."
          ,"Consider adding an account directive. Examples:"
          ,""
          ,"account %s"
          ,"account %s    ; type:A  ; (L,E,R,X,C,V)"
          ]) f l ex (show a) a a
        where
          (f,l,_mcols,ex) = makePostingErrorExcerpt p finderrcols
          -- Calculate columns suitable for highlighting the excerpt.
          -- We won't show these in the main error line as they aren't
          -- accurate for the actual data.
          finderrcols p _ _ = Just (col, Just col2)
            where
              col = 5 + if isVirtual p then 1 else 0
              col2 = col + T.length a - 1

-- | Check that all the commodities used in this journal's postings have been declared
-- by commodity directives, returning an error message otherwise.
journalCheckCommodities :: Journal -> Either String ()
journalCheckCommodities j = mapM_ checkcommodities (journalPostings j)
  where
    checkcommodities p =
      case findundeclaredcomm p of
        Nothing -> Right ()
        Just (comm, _) ->
          Left $ printf (unlines [
           "%s:%d:"
          ,"%s"
          ,"Strict commodity checking is enabled, and"
          ,"commodity %s has not been declared."
          ,"Consider adding a commodity directive. Examples:"
          ,""
          ,"commodity %s1000.00"
          ,"commodity 1.000,00 %s"
          ]) f l ex (show comm) comm comm
          where
            (f,l,_mcols,ex) = makePostingErrorExcerpt p finderrcols
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

        -- Calculate columns suitable for highlighting the excerpt.
        -- We won't show these in the main error line as they aren't
        -- accurate for the actual data.

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
journalCheckPayees :: Journal -> Either String ()
journalCheckPayees j = mapM_ checkpayee (jtxns j)
  where
    checkpayee t
      | payee `elem` journalPayeesDeclared j = Right ()
      | otherwise = Left $
        printf (unlines [
           "%s:%d:"
          ,"%s"
          ,"Strict payee checking is enabled, and"
          ,"payee %s has not been declared."
          ,"Consider adding a payee directive. Examples:"
          ,""
          ,"payee %s"
          ]) f l ex (show payee) payee
      where
        payee = transactionPayee t
        (f,l,_mcols,ex) = makeTransactionErrorExcerpt t finderrcols
        -- Calculate columns suitable for highlighting the excerpt.
        -- We won't show these in the main error line as they aren't
        -- accurate for the actual data.
        finderrcols t = Just (col, Just col2)
          where
            col  = T.length (showTransactionLineFirstPart t) + 2
            col2 = col + T.length (transactionPayee t) - 1
