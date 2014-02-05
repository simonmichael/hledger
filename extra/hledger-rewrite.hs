#!/usr/bin/env runhaskell
{-|
hledger-rewrite [PATTERNS] [--add-posting "ACCT  AMTEXPR"] ...

Skeleton for a minimal generic rewriter of journal entries.
Reads the default journal and prints the entries, like print,
but adds the specified postings to any entries matching PATTERNS.

Tested-with: hledger 0.22.2

|-}

import Hledger
import Hledger.Cli

main = do
  opts@CliOpts{reportopts_=ropts} <- getCliOpts (defCommandMode ["hledger-rewrite"])
  d <- getCurrentDay
  let
    q = queryFromOpts d ropts
    -- parse added postings from args.. hard-coded here:
    addps :: [(AccountName, Transaction -> MixedAmount)]
    addps = [
      ("(Reserve)", (\t -> (t `firstAmountMatching` q) `divideMixedAmount` 10))
      ]

  withJournalDo opts $ \opts j@Journal{jtxns=ts} -> do
      -- rewrite matched transactions
      let j' = j{jtxns=map (\t -> if q `matchesTransaction` t then rewriteTransaction t addps else t) ts}
      -- print' opts j'
      -- print all transactions (without filtering)
      putStr $ showTransactions ropts Any j'
      
rewriteTransaction :: Transaction -> [(AccountName, Transaction -> MixedAmount)] -> Transaction
rewriteTransaction t addps = t{tpostings=tpostings t ++ map (uncurry (generatePosting t)) addps}

generatePosting :: Transaction -> AccountName -> (Transaction -> MixedAmount) -> Posting
generatePosting t acct amtfn = nullposting{paccount     = accountNameWithoutPostingType acct
                                          ,ptype        = accountNamePostingType acct
                                          ,pamount      = amtfn t
                                          ,ptransaction = Just t
                                          }

firstAmountMatching :: Transaction -> Query -> MixedAmount
firstAmountMatching t q = pamount $ head $ filter (q `matchesPosting`) $ tpostings t
