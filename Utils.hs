{-|

Utilities for top-level modules. See also "Ledger.Utils".

There are some helpers here for working with your ledger in ghci. Examples:

> $ rm -f *.o Ledger/*.o
> $ ghci hledger.hs
> *Main> l <- myledger
> Ledger with 696 entries, 132 accounts:
> ...
> *Main> printbalance [] [] l
> ...
> *Main> printregister [] [] l
> ...
> *Main> accounts l
> ...
> *Main> myaccount "expenses:food:groceries"
> Account expenses:food:groceries with 60 transactions

-}

module Utils
where
import qualified Data.Map as Map (lookup)
import Options
import Ledger


-- | get a RawLedger from the given file path
rawledgerfromfile :: FilePath -> IO RawLedger
rawledgerfromfile f = do
  parsed <- parseLedgerFile f
  return $ either (\_ -> RawLedger [] [] [] "") id parsed

-- | get a cached Ledger from the given file path
ledgerfromfile :: FilePath -> IO Ledger
ledgerfromfile f = do
  l  <- rawledgerfromfile f
  return $ cacheLedger wildcard $ filterRawLedgerEntries "" "" wildcard l

-- | get a RawLedger from the file your LEDGER environment variable
-- variable points to or (WARNING) an empty one if there was a problem.
myrawledger :: IO RawLedger
myrawledger = do
  parsed <- ledgerFilePathFromOpts [] >>= parseLedgerFile
  return $ either (\_ -> RawLedger [] [] [] "") id parsed

-- | get a cached Ledger from the file your LEDGER environment variable
-- variable points to or (WARNING) an empty one if there was a problem.
myledger :: IO Ledger
myledger = do
  l <- myrawledger
  return $ cacheLedger wildcard $ filterRawLedgerEntries "" "" wildcard l

-- | get a named account from your ledger file
myaccount :: AccountName -> IO Account
myaccount a = myledger >>= (return . fromMaybe nullacct . Map.lookup a . accountmap)

