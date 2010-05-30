{-# LANGUAGE CPP #-}
{-|

Utilities for top-level modules and ghci. See also Hledger.Read and
Hledger.Data.Utils.

-}

module Hledger.Cli.Utils
    (
     withJournalDo,
     readJournalWithOpts,
     openBrowserOn
    )
where
import Hledger.Data
import Hledger.Read
import Hledger.Cli.Options (Opt(..),journalFilePathFromOpts) -- ,optsToFilterSpec)
import System.Directory (doesFileExist)
import System.Exit
import System.Info (os)
import System.Process (readProcessWithExitCode)


-- | Parse the user's specified journal file and run a hledger command on
-- it, or throw an error.
withJournalDo :: [Opt] -> [String] -> String -> ([Opt] -> [String] -> Journal -> IO ()) -> IO ()
withJournalDo opts args cmdname cmd = do
  -- We kludgily read the file before parsing to grab the full text, unless
  -- it's stdin, or it doesn't exist and we are adding. We read it strictly
  -- to let the add command work.
  f <- journalFilePathFromOpts opts
  fileexists <- doesFileExist f
  let creating = not fileexists && cmdname == "add"
      costify = (if CostBasis `elem` opts then journalConvertAmountsToCost else id)
      runcmd = cmd opts args . costify
  if creating
   then runcmd nulljournal
   else readJournalFile f >>= runcmd

-- | Get a journal from the given string and options, or throw an error.
readJournalWithOpts :: [Opt] -> String -> IO Journal
readJournalWithOpts opts s = do
    j <- readJournal s
    let cost = CostBasis `elem` opts
    return $ (if cost then journalConvertAmountsToCost else id) j

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

