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

-- | Get a Ledger from the given string and options, or raise an error.
ledgerfromstringwithopts :: [Opt] -> [String] -> Day -> String -> Ledger
ledgerfromstringwithopts opts args refdate s =
    prepareLedger opts args refdate $ rawledgerfromstring s

-- | Get a Ledger from the given file path and options, or raise an error.
ledgerfromfilewithopts :: [Opt] -> [String] -> FilePath -> IO Ledger
ledgerfromfilewithopts opts args f = do
    rl <- readFile f >>= return . rawledgerfromstring
    refdate <- today
    return $ prepareLedger opts args refdate rl
           
-- | Get a Ledger from your default ledger file, or raise an error.
-- Assumes no options.
myledger :: IO Ledger
myledger = ledgerFilePathFromOpts [] >>= ledgerfromfilewithopts [] []
