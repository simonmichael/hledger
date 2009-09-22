{-|

Utilities for top-level modules and ghci. See also "Ledger.IO" and
"Ledger.Utils".

-}

module Utils
where
import Control.Monad.Error
import Ledger
import Options (Opt,ledgerFilePathFromOpts,optsToFilterSpec)
import System.Directory (doesFileExist)
import System.IO
import System.Exit
import System.Cmd (system)
import System.Info (os)
import System.Time (getClockTime)


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
  tc <- getClockTime
  let go = cmd opts args . filterAndCacheLedgerWithOpts opts args t rawtext . (\rl -> rl{filepath=f,filereadtime=tc})
  case creating of
    True -> go rawLedgerEmpty
    False -> (runErrorT . parseLedgerFile t) f >>= either (hPutStrLn stderr) go

-- | Get a Ledger from the given string and options, or raise an error.
ledgerFromStringWithOpts :: [Opt] -> [String] -> LocalTime -> String -> IO Ledger
ledgerFromStringWithOpts opts args reftime s =
    liftM (filterAndCacheLedgerWithOpts opts args reftime s) $ rawLedgerFromString s

-- | Read a Ledger from the given file, filtering according to the
-- options, or give an error.
readLedgerWithOpts :: [Opt] -> [String] -> FilePath -> IO Ledger
readLedgerWithOpts opts args f = do
  t <- getCurrentLocalTime
  readLedgerWithFilterSpec (optsToFilterSpec opts args t) f
           
-- | Convert a RawLedger to a canonicalised, cached and filtered Ledger
-- based on the command-line options/arguments and a reference time.
filterAndCacheLedgerWithOpts ::  [Opt] -> [String] -> LocalTime -> String -> RawLedger -> Ledger
filterAndCacheLedgerWithOpts opts args = filterAndCacheLedger . optsToFilterSpec opts args

-- | Attempt to open a web browser on the given url, all platforms.
openBrowserOn :: String -> IO ExitCode
openBrowserOn u = trybrowsers browsers u
    where
      trybrowsers (b:bs) u = do
        e <- system $ printf "%s %s" b u
        case e of
          ExitSuccess -> return ExitSuccess
          ExitFailure _ -> trybrowsers bs u
      trybrowsers [] u = do
        putStrLn $ printf "Sorry, I could not start a browser (tried: %s)" $ intercalate ", " browsers
        putStrLn $ printf "Please open your browser and visit %s" u
        return $ ExitFailure 127
      browsers | os=="darwin"  = ["open"]
               | os=="mingw32" = ["start","firefox","safari","opera","iexplore"]
               | otherwise     = ["sensible-browser","firefox"]
    -- jeffz: write a ffi binding for it using the Win32 package as a basis
    -- start by adding System/Win32/Shell.hsc and follow the style of any
    -- other module in that directory for types, headers, error handling and
    -- what not.
    -- ::ShellExecute(NULL, "open", "www.somepage.com", NULL, NULL, SW_SHOWNORMAL);
    -- ::ShellExecute(NULL, "open", "firefox.exe", "www.somepage.com" NULL, SW_SHOWNORMAL);

