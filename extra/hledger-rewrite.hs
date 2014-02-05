#!/usr/bin/env runhaskell
{-|
hledger-rewrite PATTERNS --add-posting "ACCT  AMTEXPR" ...

Skeleton for a minimal generic rewriter of journal entries.
Reads the default journal and prints the entries, like print,
but adds the specified postings to any entries matching PATTERNS.

|-}

import Hledger.Data.Types (Journal(..))
import Hledger
import Hledger.Cli

main = do
  putStrLn "(-f option not supported)"
  opts <- getCliOpts (defCommandMode ["hledger-rewrite"])
  withJournalDo opts $
    \opts j@Journal{jtxns=ts} -> print' opts j{jtxns=map rewrite ts}
    where 
      rewrite t = if matched t then t{tpostings=tpostings t ++ newps t}
                               else t
      matched t = Acct "^income" `matchesTransaction` t
      newps t = [generatePosting t "(Reserve)" (`divideMixedAmount` 10)]
      generatePosting t acct amtfn = nullposting{paccount = accountNameWithoutPostingType acct
                                                ,ptype    = accountNamePostingType acct
                                                ,pamount  = amtfn amt
                                                }
        where
          amt = pamount $ head $ filter (Acct "^income" `matchesPosting`) $ tpostings t
