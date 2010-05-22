{-# LANGUAGE CPP #-}
{-|

Utilities for top-level modules and ghci. See also Hledger.Data.IO and
Hledger.Data.Utils.

-}

module Hledger.Utils
where
import Control.Monad.Error
import Hledger.Data
import Hledger.Cli.Options (Opt(..),ledgerFilePathFromOpts) -- ,optsToFilterSpec)
import System.Directory (doesFileExist)
import System.IO (stderr)
#if __GLASGOW_HASKELL__ <= 610
import System.IO.UTF8 (hPutStrLn)
#else
import System.IO (hPutStrLn)
#endif
import System.Exit
import System.Process (readProcessWithExitCode)
import System.Info (os)
import System.Time (getClockTime)


-- | Parse the user's specified ledger file and run a hledger command on
-- it, or report a parse error. This function makes the whole thing go.
-- Warning, this provides only an uncached/unfiltered ledger, so the
-- command should do further processing if needed.
withLedgerDo :: [Opt] -> [String] -> String -> ([Opt] -> [String] -> UncachedLedger -> IO ()) -> IO ()
withLedgerDo opts args cmdname cmd = do
  -- We kludgily read the file before parsing to grab the full text, unless
  -- it's stdin, or it doesn't exist and we are adding. We read it strictly
  -- to let the add command work.
  f <- ledgerFilePathFromOpts opts
  let f' = if f == "-" then "/dev/null" else f
  fileexists <- doesFileExist f
  let creating = not fileexists && cmdname == "add"
      cb = CostBasis `elem` opts
  t <- getCurrentLocalTime
  tc <- getClockTime
  txt <-  if creating then return "" else strictReadFile f'
  let runcmd = cmd opts args . makeUncachedLedger cb f tc txt
  if creating
   then runcmd nulljournal
   else (runErrorT . parseLedgerFile t) f >>= either parseerror runcmd
    where parseerror e = hPutStrLn stderr e >> exitWith (ExitFailure 1)

-- | Get an uncached ledger from the given string and options, or raise an error.
ledgerFromStringWithOpts :: [Opt] -> String -> IO UncachedLedger
ledgerFromStringWithOpts opts s = do
    tc <- getClockTime
    j <- journalFromString s
    let cb = CostBasis `elem` opts
    return $ makeUncachedLedger cb "" tc s j

-- -- | Read a ledger from the given file, or give an error.
-- readLedgerWithOpts :: [Opt] -> [String] -> FilePath -> IO Ledger
-- readLedgerWithOpts opts args f = do
--   t <- getCurrentLocalTime
--   readLedger f
           
-- -- | Convert a Journal to a canonicalised, cached and filtered Ledger
-- -- based on the command-line options/arguments and a reference time.
-- filterAndCacheLedgerWithOpts ::  [Opt] -> [String] -> LocalTime -> String -> Journal -> Ledger
-- filterAndCacheLedgerWithOpts opts args = filterAndCacheLedger . optsToFilterSpec opts args

-- | Attempt to open a web browser on the given url, all platforms.
openBrowserOn :: String -> IO ExitCode
openBrowserOn u = trybrowsers browsers u
    where
      trybrowsers (b:bs) u = do
        (e,_,_) <- readProcessWithExitCode b [u] ""
        case e of
          ExitSuccess -> return ExitSuccess
          ExitFailure _ -> trybrowsers bs u
      trybrowsers [] u = do
        putStrLn $ printf "Sorry, I could not start a browser (tried: %s)" $ intercalate ", " browsers
        putStrLn $ printf "Please open your browser and visit %s" u
        return $ ExitFailure 127
      browsers | os=="darwin"  = ["open"]
               | os=="mingw32" = ["start"]
               | otherwise     = ["sensible-browser","gnome-www-browser","firefox"]
    -- jeffz: write a ffi binding for it using the Win32 package as a basis
    -- start by adding System/Win32/Shell.hsc and follow the style of any
    -- other module in that directory for types, headers, error handling and
    -- what not.
    -- ::ShellExecute(NULL, "open", "www.somepage.com", NULL, NULL, SW_SHOWNORMAL);
    -- ::ShellExecute(NULL, "open", "firefox.exe", "www.somepage.com" NULL, SW_SHOWNORMAL);

