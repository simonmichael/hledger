{-|

Utilities for top-level modules and ghci. See also "Ledger.IO" and
"Ledger.Utils".

-}

module Utils
where
import Control.Monad.Error
import Data.Time.Clock
import Ledger
import Options (Opt,ledgerFilePathFromOpts,optsToIOArgs)
import System.IO
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map (lookup)


-- | parse the user's specified ledger file and run a hledger command on it,
-- or report a parse error. This function makes the whole thing go.
withLedgerDo :: [Opt] -> [String] -> ([Opt] -> [String] -> Ledger -> IO ()) -> IO ()
withLedgerDo opts args cmd = do
  f <- ledgerFilePathFromOpts opts
  -- kludgily read the file a second time to get the full text. Only the ui command needs it.
  -- kludgily try not to fail if it's stdin.
  -- read it strictly to let the add command work
  rawtext <- strictReadFile $ if f == "-" then "/dev/null" else f
  t <- getCurrentLocalTime
  let runcmd = cmd opts args . filterAndCacheLedgerWithOpts opts args t rawtext . (\rl -> rl{filepath=f})
  return f >>= runErrorT . parseLedgerFile t >>= either (hPutStrLn stderr) runcmd

-- | Get a Ledger from the given string and options, or raise an error.
ledgerFromStringWithOpts :: [Opt] -> [String] -> LocalTime -> String -> IO Ledger
ledgerFromStringWithOpts opts args reftime s =
    liftM (filterAndCacheLedgerWithOpts opts args reftime s) $ rawLedgerFromString s

-- | Read a Ledger from the given file, filtering according to the
-- options, or give an error.
readLedgerWithOpts :: [Opt] -> [String] -> FilePath -> IO Ledger
readLedgerWithOpts opts args f = do
  t <- getCurrentLocalTime
  readLedgerWithIOArgs (optsToIOArgs opts args t) f
           
-- | Convert a RawLedger to a canonicalised, cached and filtered Ledger
-- based on the command-line options/arguments and a reference time.
filterAndCacheLedgerWithOpts ::  [Opt] -> [String] -> LocalTime -> String -> RawLedger -> Ledger
filterAndCacheLedgerWithOpts opts args t = filterAndCacheLedger (optsToIOArgs opts args t)

