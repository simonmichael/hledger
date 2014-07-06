{-# LANGUAGE ScopedTypeVariables, CPP #-}
{-|

Utilities for top-level modules and ghci. See also Hledger.Read and
Hledger.Utils.

-}

module Hledger.Cli.Utils
    (
     withJournalDo,
     journalReload,
     journalReloadIfChanged,
     journalFileIsNewer,
     journalSpecifiedFileIsNewer,
     fileModificationTime,
     openBrowserOn,
     writeFileWithBackup,
     writeFileWithBackupIfChanged,
     readFileStrictly,
     Test(TestList),
    )
where
import Control.Exception as C
import Data.List
import Data.Maybe
import Safe (readMay)
import System.Console.CmdArgs
import System.Directory (getModificationTime, getDirectoryContents, copyFile)
import System.Exit
import System.FilePath ((</>), splitFileName, takeDirectory)
import System.Info (os)
import System.Process (readProcessWithExitCode)
import System.Time (ClockTime, getClockTime, diffClockTimes, TimeDiff(TimeDiff))
import Test.HUnit
import Text.Printf
import Text.Regex.TDFA (match)


-- kludge - adapt to whichever directory version is installed, or when
-- cabal macros aren't available, assume the new directory
#ifdef MIN_VERSION_directory
#if MIN_VERSION_directory(1,2,0)
#define directory_1_2
#endif
#else
#define directory_1_2
#endif

#ifdef directory_1_2
import System.Time (ClockTime(TOD))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
#endif

import Hledger.Cli.Options
import Hledger.Data
import Hledger.Read
import Hledger.Utils


-- | Parse the user's specified journal file and run a hledger command on
-- it, or throw an error.
withJournalDo :: CliOpts -> (CliOpts -> Journal -> IO ()) -> IO ()
withJournalDo opts cmd = do
  -- We kludgily read the file before parsing to grab the full text, unless
  -- it's stdin, or it doesn't exist and we are adding. We read it strictly
  -- to let the add command work.
  rulespath <- rulesFilePathFromOpts opts
  journalpath <- journalFilePathFromOpts opts
  ej <- readJournalFile Nothing rulespath (not $ ignore_assertions_ opts) journalpath
  either error' (cmd opts . journalApplyAliases (aliasesFromOpts opts)) ej

-- -- | Get a journal from the given string and options, or throw an error.
-- readJournalWithOpts :: CliOpts -> String -> IO Journal
-- readJournalWithOpts opts s = readJournal Nothing Nothing Nothing s >>= either error' return

-- | Re-read a journal from its data file, or return an error string.
journalReload :: Journal -> IO (Either String Journal)
journalReload j = readJournalFile Nothing Nothing True $ journalFilePath j

-- | Re-read a journal from its data file mostly, only if the file has
-- changed since last read (or if there is no file, ie data read from
-- stdin). The provided options are mostly ignored. Return a journal or
-- the error message while reading it, and a flag indicating whether it
-- was re-read or not.
journalReloadIfChanged :: CliOpts -> Journal -> IO (Either String Journal, Bool)
journalReloadIfChanged _ j = do
  let maybeChangedFilename f = do newer <- journalSpecifiedFileIsNewer j f
                                  return $ if newer then Just f else Nothing
  changedfiles <- catMaybes `fmap` mapM maybeChangedFilename (journalFilePaths j)
  if not $ null changedfiles
   then do
     whenLoud $ printf "%s has changed, reloading\n" (head changedfiles)
     jE <- journalReload j
     return (jE, True)
   else
     return (Right j, False)

-- | Has the journal's main data file changed since the journal was last
-- read ?
journalFileIsNewer :: Journal -> IO Bool
journalFileIsNewer j@Journal{filereadtime=tread} = do
  tmod <- fileModificationTime $ journalFilePath j
  return $ diffClockTimes tmod tread > (TimeDiff 0 0 0 0 0 0 0)

-- | Has the specified file (presumably one of journal's data files)
-- changed since journal was last read ?
journalSpecifiedFileIsNewer :: Journal -> FilePath -> IO Bool
journalSpecifiedFileIsNewer Journal{filereadtime=tread} f = do
  tmod <- fileModificationTime f
  return $ diffClockTimes tmod tread > (TimeDiff 0 0 0 0 0 0 0)

-- | Get the last modified time of the specified file, or if it does not
-- exist or there is some other error, the current time.
fileModificationTime :: FilePath -> IO ClockTime
fileModificationTime f
    | null f = getClockTime
    | otherwise = (do
#ifdef directory_1_2
        utc <- getModificationTime f
        let nom = utcTimeToPOSIXSeconds utc
        let clo = TOD (read $ takeWhile (`elem` "0123456789") $ show nom) 0 -- XXX read
#else
        clo <- getModificationTime f
#endif
        return clo
        )
        `C.catch` \(_::C.IOException) -> getClockTime
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
        putStrLn $ printf "Could not start a web browser (tried: %s)" $ intercalate ", " browsers
        putStrLn $ printf "Please open your browser and visit %s" u
        return $ ExitFailure 127
      browsers | os=="darwin"  = ["open"]
               | os=="mingw32" = ["c:/Program Files/Mozilla Firefox/firefox.exe"]
               | otherwise     = ["sensible-browser","gnome-www-browser","firefox"]
    -- jeffz: write a ffi binding for it using the Win32 package as a basis
    -- start by adding System/Win32/Shell.hsc and follow the style of any
    -- other module in that directory for types, headers, error handling and
    -- what not.
    -- ::ShellExecute(NULL, "open", "www.somepage.com", NULL, NULL, SW_SHOWNORMAL);

-- | Back up this file with a (incrementing) numbered suffix then
-- overwrite it with this new text, or give an error, but only if the text
-- is different from the current file contents, and return a flag
-- indicating whether we did anything.
writeFileWithBackupIfChanged :: FilePath -> String -> IO Bool
writeFileWithBackupIfChanged f t = do
  s <- readFile' f
  if t == s then return False
            else backUpFile f >> writeFile f t >> return True

-- | Back up this file with a (incrementing) numbered suffix, then
-- overwrite it with this new text, or give an error.
writeFileWithBackup :: FilePath -> String -> IO ()
writeFileWithBackup f t = backUpFile f >> writeFile f t

readFileStrictly :: FilePath -> IO String
readFileStrictly f = readFile' f >>= \s -> C.evaluate (length s) >> return s

-- | Back up this file with a (incrementing) numbered suffix, or give an error.
backUpFile :: FilePath -> IO ()
backUpFile fp = do
  fs <- safeGetDirectoryContents $ takeDirectory $ fp
  let (d,f) = splitFileName fp
      versions = catMaybes $ map (f `backupNumber`) fs
      next = maximum (0:versions) + 1
      f' = printf "%s.%d" f next
  copyFile fp (d </> f')

safeGetDirectoryContents :: FilePath -> IO [FilePath]
safeGetDirectoryContents "" = getDirectoryContents "."
safeGetDirectoryContents fp = getDirectoryContents fp

-- | Does the second file represent a backup of the first, and if so which version is it ?
-- XXX nasty regex types intruding, add a simpler api to Hledger.Utils.Regex
backupNumber :: FilePath -> FilePath -> Maybe Int
backupNumber f g = case match (toRegex ("^" ++ f ++ "\\.([0-9]+)$")) g of
                        (_::FilePath, _::FilePath, _::FilePath, [ext::FilePath]) -> readMay ext
                        _ -> Nothing
