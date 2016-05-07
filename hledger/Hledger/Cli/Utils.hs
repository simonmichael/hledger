{-# LANGUAGE ScopedTypeVariables, CPP #-}
{-|

Utilities for top-level modules and ghci. See also Hledger.Read and
Hledger.Utils.

-}

module Hledger.Cli.Utils
    (
     withJournalDo,
     writeOutput,
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
import Data.Time (Day)
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
import Text.Regex.TDFA ((=~))


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

import Hledger.Cli.CliOptions
import Hledger.Data
import Hledger.Read
import Hledger.Reports (queryFromOpts)
import Hledger.Utils


-- | Parse the user's specified journal file and run a hledger command on
-- it, or throw an error.
withJournalDo :: CliOpts -> (CliOpts -> Journal -> IO ()) -> IO ()
withJournalDo opts cmd = do
  -- We kludgily read the file before parsing to grab the full text, unless
  -- it's stdin, or it doesn't exist and we are adding. We read it strictly
  -- to let the add command work.
  rulespath <- rulesFilePathFromOpts opts
  journalpaths <- journalFilePathFromOpts opts
  ej <- readJournalFiles Nothing rulespath (not $ ignore_assertions_ opts) journalpaths
  either error' (cmd opts . pivotByOpts opts . journalApplyAliases (aliasesFromOpts opts)) ej

-- | Apply the pivot transformation on a journal, if option is present.
pivotByOpts :: CliOpts -> Journal -> Journal
pivotByOpts opts
  | Just tag <- maybeTag = pivot tag
  | Nothing  <- maybeTag = id
 where maybeTag = maybestringopt "pivot" . rawopts_ $ opts

-- | Apply the pivot transformation by given tag on a journal.
pivot :: String -> Journal -> Journal
pivot tag j = j{jtxns = map pivotTrans . jtxns $ j}
 where
  pivotTrans t = t{tpostings = map pivotPosting . tpostings $ t}
  pivotPosting p
    | Just (_ , value) <- tagTuple = p{paccount = joinAccountNames tag value}
    | _                <- tagTuple = p
   where tagTuple = find ((tag ==) . fst) . ptags $ p

-- | Write some output to stdout or to a file selected by --output-file.
writeOutput :: CliOpts -> String -> IO ()
writeOutput opts s = do
  f <- outputFileFromOpts opts
  (if f == "-" then putStr else writeFile f) s
  
-- -- | Get a journal from the given string and options, or throw an error.
-- readJournalWithOpts :: CliOpts -> String -> IO Journal
-- readJournalWithOpts opts s = readJournal Nothing Nothing Nothing s >>= either error' return

-- | Re-read the journal file(s) specified by options, or return an error string.
-- Options are honoured and the provided date is used as the current date.
journalReload :: CliOpts -> IO (Either String Journal)
journalReload opts = do
  rulespath <- rulesFilePathFromOpts opts
  journalpaths <- journalFilePathFromOpts opts
  readJournalFiles Nothing rulespath (not $ ignore_assertions_ opts) journalpaths

-- | Re-read the journal file(s) specified by options, but only if any
-- of them has changed since last read (or if there is no file, ie
-- data read from stdin). The provided options and current date are
-- used to filter the re-read journal; this is intended to reapply the
-- same filter as at program startup (though, the current date may not
-- be what it was then, so results may differ). Returns a journal or
-- error message, and a flag indicating whether it was re-read or not.
journalReloadIfChanged :: CliOpts -> Day -> Journal -> IO (Either String Journal, Bool)
journalReloadIfChanged opts d j = do
  let maybeChangedFilename f = do newer <- journalSpecifiedFileIsNewer j f
                                  return $ if newer then Just f else Nothing
  changedfiles <- catMaybes `fmap` mapM maybeChangedFilename (journalFilePaths j)
  if not $ null changedfiles
   then do
     whenLoud $ printf "%s has changed, reloading\n" (head changedfiles)
     ej <- journalReload opts
     let initq = queryFromOpts d $ reportopts_ opts
         ej'   = filterJournalTransactions initq <$> ej
     return (ej', True)
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
backupNumber f g = case g =~ ("^" ++ f ++ "\\.([0-9]+)$") of
                        (_::FilePath, _::FilePath, _::FilePath, [ext::FilePath]) -> readMay ext
                        _ -> Nothing
