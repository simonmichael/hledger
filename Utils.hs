{-|

Utilities for top-level modules and ghci. See also "Ledger.IO" and
"Ledger.Utils".

-}

module Utils
where
import Control.Monad.Error
import Ledger
import Options (Opt,ledgerFilePathFromOpts,optsToIOArgs)
import System.Directory (doesFileExist)
import System.IO


-- | Parse the user's specified ledger file and run a hledger command on
-- it, or report a parse error. This function makes the whole thing go.
withLedgerDo :: [Opt] -> [String] -> String -> ([Opt] -> [String] -> Ledger -> IO ()) -> IO ()
withLedgerDo opts args cmdname cmd = do
  -- We kludgily read the file before parsing to grab the full text, unless
  -- it's stdin, or it doesn't exist and we are adding. We read it strictly
  -- to let the add command work.
  f <- ledgerFilePathFromOpts opts
  let f' = if f == "-" then "/dev/null" else f
  fileexists <- doesFileExist f
  let creating = not fileexists && cmdname == "add"
  rawtext <-  if creating then return "" else strictReadFile f'
  t <- getCurrentLocalTime
  let go = cmd opts args . filterAndCacheLedgerWithOpts opts args t rawtext . (\rl -> rl{filepath=f})
  case creating of
    True -> return rawLedgerEmpty >>= go
    False -> return f >>= runErrorT . parseLedgerFile t >>= either (hPutStrLn stderr) go

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

