{-|

Utilities for top-level modules and/or ghci. See also "Ledger.Utils".

-}

module Utils
where
import qualified Data.Map as Map (lookup)
import Text.ParserCombinators.Parsec
import System.IO
import Options
import Ledger


-- | Convert a RawLedger to a canonicalised, cached and filtered Ledger
-- based on the command-line options/arguments and today's date.
prepareLedger ::  [Opt] -> [String] -> Day -> RawLedger -> Ledger
prepareLedger opts args refdate rl =
    cacheLedger apats $ filterRawLedger span dpats c r $ canonicaliseAmounts cb rl
    where
      (apats,dpats) = parseAccountDescriptionArgs [] args
      span = dateSpanFromOpts refdate opts
      c = Cleared `elem` opts
      r = Real `elem` opts
      cb = CostBasis `elem` opts

-- | Get a RawLedger from the given string, or raise an error.
rawledgerfromstring :: String -> RawLedger
rawledgerfromstring = fromparse . parsewith ledgerfile

-- | Get a RawLedger from the given file path, or raise an error.
rawledgerfromfile :: FilePath -> IO RawLedger
rawledgerfromfile f = openFile f ReadMode >>= hGetContents >>= return . rawledgerfromstring

-- | Get a RawLedger from the file your LEDGER environment variable
-- variable points to, or raise an error.
myrawledger :: IO RawLedger
myrawledger = ledgerFilePathFromOpts [] >>= rawledgerfromfile

-- | Get a filtered and cached Ledger from the given string and arguments,
-- or raise an error.  Does not handle all the command-line options that
-- parseLedgerAndDo does.
ledgerfromstring :: [String] -> String -> Ledger
ledgerfromstring args s =
    cacheLedger apats $ filterRawLedger span dpats False False $ canonicaliseAmounts False l
    where
      (apats,dpats) = parseAccountDescriptionArgs [] args
      span = (DateSpan Nothing Nothing)
      l = rawledgerfromstring s
           
ledgerfromstringwithopts :: [Opt] -> [String] -> Day -> String -> Ledger
ledgerfromstringwithopts opts args refdate s =
    prepareLedger opts args refdate $ rawledgerfromstring s

-- | Get a filtered and cached Ledger from the given file path and
-- arguments, or raise an error.  Does not handle all the command-line
-- options that parseLedgerAndDo does.
ledgerfromfile :: [String] -> FilePath -> IO Ledger
ledgerfromfile args f =
    rawledgerfromfile f >>= return .
    cacheLedger apats . 
    filterRawLedger (DateSpan Nothing Nothing) dpats False False .
    canonicaliseAmounts False
      where
        (apats,dpats) = parseAccountDescriptionArgs [] args
           
-- | Get a cached Ledger from the file your LEDGER environment variable
-- variable points to, or raise an error. Assumes no command-line arguments.
myledger :: IO Ledger
myledger = myrawledger >>= return .
           cacheLedger [] . 
           filterRawLedger (DateSpan Nothing Nothing) [] False False .
           canonicaliseAmounts False

-- | Get a named account from your ledger file.
myaccount :: AccountName -> IO Account
myaccount a = myledger >>= (return . fromMaybe nullacct . Map.lookup a . accountmap)

