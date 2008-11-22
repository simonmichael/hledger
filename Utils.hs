{-|

Utilities for top-level modules and/or ghci. See also "Ledger.Utils".

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

-- | get a cached Ledger from the given file path, filtered by the patterns.
ledgerfromfile :: [String] -> FilePath -> IO Ledger
ledgerfromfile args f = do
  l  <- rawledgerfromfile f
  return $ cacheLedger apats $ filterRawLedger Nothing Nothing dpats False False l
      where
        (apats,dpats) = parseAccountDescriptionArgs args
           
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
  return $ cacheLedger [] $ filterRawLedger Nothing Nothing [] False False l

-- | get a named account from your ledger file
myaccount :: AccountName -> IO Account
myaccount a = myledger >>= (return . fromMaybe nullacct . Map.lookup a . accountmap)

