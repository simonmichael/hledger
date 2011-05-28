#!/usr/bin/env runhaskell
-- list the default journal's chart of accounts in --flat style
import Hledger
main = do
  j <- myJournal
  let l = journalToLedger nullfilterspec{empty=True} j
  mapM_ putStrLn (accountnames l)
