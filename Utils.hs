{-|

Utilities for top-level modules and/or ghci. See also "Ledger.Utils".

-}

module Utils
where
import qualified Data.Map as Map (lookup)
import Text.ParserCombinators.Parsec
import Options
import Ledger


-- | Get a RawLedger from the given string, or raise an error.
rawledgerfromstring :: String -> RawLedger
rawledgerfromstring = fromparse . parsewith ledgerfile

-- | Get a filtered and cached Ledger from the given string, or raise an error.
ledgerfromstring :: [String] -> String -> Ledger
ledgerfromstring args s =
  cacheLedger apats $ filterRawLedger Nothing Nothing dpats False False l
      where
        (apats,dpats) = parseAccountDescriptionArgs args
        l = rawledgerfromstring s
           
-- | Get a RawLedger from the given file path, or a dummy one if there was an error.
rawledgerfromfile :: FilePath -> IO RawLedger
rawledgerfromfile f = do
  parsed <- parseLedgerFile f
  return $ either (\_ -> RawLedger [] [] [] "") id parsed

-- | Get a filtered and cached Ledger from the given file path, or a dummy
-- one if there was an error.
ledgerfromfile :: [String] -> FilePath -> IO Ledger
ledgerfromfile args f = do
  l  <- rawledgerfromfile f
  return $ cacheLedger apats $ filterRawLedger Nothing Nothing dpats False False l
      where
        (apats,dpats) = parseAccountDescriptionArgs args
           
-- | Get a RawLedger from the file your LEDGER environment variable
-- variable points to, or a dummy one if there was a problem.
myrawledger :: IO RawLedger
myrawledger = do
  parsed <- ledgerFilePathFromOpts [] >>= parseLedgerFile
  return $ either (\_ -> RawLedger [] [] [] "") id parsed

-- | Get a cached Ledger from the file your LEDGER environment variable
-- variable points to, or a dummy one if there was a problem.
myledger :: IO Ledger
myledger = do
  l <- myrawledger
  return $ cacheLedger [] $ filterRawLedger Nothing Nothing [] False False l

-- | Get a named account from your ledger file.
myaccount :: AccountName -> IO Account
myaccount a = myledger >>= (return . fromMaybe nullacct . Map.lookup a . accountmap)

