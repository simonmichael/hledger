#!/usr/bin/env runhaskell
-- Show all account names used in the default journal.

import Hledger.Cli

main = do
  -- simple way to read ~/.hledger.journal or $LEDGER_FILE
  -- j <- defaultJournal

  -- but we'd better handle -f as well:
  opts <- getCliOpts $ mainmode []
  withJournalDo opts $ \_opts j -> do

  -- query the journal for all account names and print each one
  mapM_ putStrLn $ journalAccountNames j
