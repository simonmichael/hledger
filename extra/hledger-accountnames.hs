#!/usr/bin/env runhaskell
-- Show all account names used in the default journal.

import Hledger

main = do
  j <- defaultJournal
  let l = ledgerFromJournal Any j
  mapM_ putStrLn $ ledgerAccountNames l
