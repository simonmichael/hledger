{-# LANGUAGE ScopedTypeVariables, CPP #-}
{-|

Utilities for top-level modules and ghci. See also Hledger.Read and
Hledger.Utils.

-}

module Hledger.Cli.Utils
    (
     withJournalDo,
     writeOutput,
     journalTransform,
     journalApplyValue, 
     journalAddForecast,
     generateAutomaticPostings,
     journalReload,
     journalReloadIfChanged,
     journalFileIsNewer,
     journalSpecifiedFileIsNewer,
     fileModificationTime,
     openBrowserOn,
     writeFileWithBackup,
     writeFileWithBackupIfChanged,
     readFileStrictly,
     pivotByOpts,
     anonymiseByOpts,
     Test(TestList),
    )
where
import Control.Exception as C
import Control.Monad

import Data.Hashable (hash)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (Day, addDays)
import Data.Word
import Numeric
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

import System.Time (ClockTime(TOD))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Hledger.Cli.CliOptions
import Hledger.Data
import Hledger.Read
import Hledger.Reports
import Hledger.Utils

-- | Parse the user's specified journal file(s) as a Journal, maybe apply some
-- transformations according to options, and run a hledger command with it. 
-- Or, throw an error.
withJournalDo :: CliOpts -> (CliOpts -> Journal -> IO ()) -> IO ()
withJournalDo opts cmd = do
  -- We kludgily read the file before parsing to grab the full text, unless
  -- it's stdin, or it doesn't exist and we are adding. We read it strictly
  -- to let the add command work.
  journalpaths <- journalFilePathFromOpts opts
  readJournalFiles (inputopts_ opts) journalpaths 
  >>= mapM (journalTransform opts)
  >>= either error' (cmd opts)

-- | Apply some transformations to the journal if specified by options.
-- These include:
-- 
-- - adding forecast transactions (--forecast)
-- - converting amounts to market value (--value)
-- - pivoting account names (--pivot)
-- - anonymising (--anonymise).
journalTransform :: CliOpts -> Journal -> IO Journal
journalTransform opts@CliOpts{reportopts_=ropts} =
      journalAddForecast opts
  >=> journalApplyValue ropts
  >=> return . pivotByOpts opts
  >=> return . anonymiseByOpts opts

-- | Apply the pivot transformation on a journal, if option is present.
pivotByOpts :: CliOpts -> Journal -> Journal
pivotByOpts opts =
  case maybestringopt "pivot" . rawopts_ $ opts of
    Just tag -> journalPivot $ T.pack tag
    Nothing  -> id

-- | Apply the anonymisation transformation on a journal, if option is present
anonymiseByOpts :: CliOpts -> Journal -> Journal
anonymiseByOpts opts =
  case maybestringopt "anon" . rawopts_ $ opts of
    Just _  -> anonymise
    Nothing -> id

-- | Apply the anonymisation transformation on a journal
anonymise :: Journal -> Journal
anonymise j
  = let
      pAnons p = p { paccount = T.intercalate (T.pack ":") . map anon . T.splitOn (T.pack ":") . paccount $ p
                   , pcomment = T.empty
                   , ptransaction = fmap tAnons . ptransaction $ p
                   }
      tAnons txn = txn { tpostings = map pAnons . tpostings $ txn
                       , tdescription = anon . tdescription $ txn
                       , tcomment = T.empty
                       }
    in
      j { jtxns = map tAnons . jtxns $ j }
  where
    anon = T.pack . flip showHex "" . (fromIntegral :: Int -> Word32) . hash

-- TODO move journalApplyValue and friends to Hledger.Data.Journal ? They are here because they use ReportOpts

-- | If -V/--value was requested, convert all journal amounts to their market value
-- as of the report end date. Cf http://hledger.org/manual.html#market-value
-- Since 2017/4 we do this early, before commands run, which affects all commands
-- and seems to have the same effect as doing it last on the reported values.
journalApplyValue :: ReportOpts -> Journal -> IO Journal
journalApplyValue ropts j = do
    today <- getCurrentDay
    mspecifiedenddate <- specifiedEndDate ropts
    let d = fromMaybe today mspecifiedenddate
        convert | value_ ropts = overJournalAmounts (amountValue j d)
                | otherwise    = id
    return $ convert j

-- | Generate periodic transactions from all periodic transaction rules in the journal.
-- These transactions are added to the in-memory Journal (but not the on-disk file).
--
-- They start on or after the day following the latest normal transaction in the journal,
-- or today if there are none.
-- They end on or before the specified report end date, or 180 days from today if unspecified.
--
journalAddForecast :: CliOpts -> Journal -> IO Journal
journalAddForecast opts@CliOpts{reportopts_=ropts} j = do
  today <- getCurrentDay

  -- "They start on or after the day following the latest normal transaction in the journal, or today if there are none."
  let DateSpan _ mjournalend = dbg2 "journalspan"   $ journalDateSpan False j  -- ignore secondary dates
      forecaststart          = dbg2 "forecaststart" $ fromMaybe today mjournalend

  -- "They end on or before the specified report end date, or 180 days from today if unspecified."
  mspecifiedend <-  snd . dbg2 "specifieddates" <$> specifiedStartEndDates ropts
  let forecastend = dbg2 "forecastend" $ fromMaybe (addDays 180 today) mspecifiedend

  let forecastspan = DateSpan (Just forecaststart) (Just forecastend)
      forecasttxns = [ txnTieKnot t | pt <- jperiodictxns j
                                    , t <- runPeriodicTransaction pt forecastspan
                                    , spanContainsDate forecastspan (tdate t)
                                    ]
  return $
    if forecast_ ropts 
      then journalBalanceTransactions' opts j{ jtxns = forecasttxns ++ jtxns j }  -- XXX wouldn't appending be better ?
      else j
  where      
    journalBalanceTransactions' opts j =
      let assrt = not . ignore_assertions_ $ inputopts_ opts
      in
       either error' id $ journalBalanceTransactions assrt j

-- | Write some output to stdout or to a file selected by --output-file.
-- If the file exists it will be overwritten.
writeOutput :: CliOpts -> String -> IO ()
writeOutput opts s = do
  f <- outputFileFromOpts opts
  (if f == "-" then putStr else writeFile f) s
  
-- -- | Get a journal from the given string and options, or throw an error.
-- readJournal :: CliOpts -> String -> IO Journal
-- readJournal opts s = readJournal def Nothing s >>= either error' return

-- | Re-read the journal file(s) specified by options, applying any
-- transformations specified by options. Or return an error string.
-- Reads the full journal, without filtering.
journalReload :: CliOpts -> IO (Either String Journal)
journalReload opts = do
  journalpaths <- journalFilePathFromOpts opts
  readJournalFiles (inputopts_ opts) journalpaths
  >>= mapM (journalTransform opts)

-- | Re-read the option-specified journal file(s), but only if any of
-- them has changed since last read. (If the file is standard input,
-- this will either do nothing or give an error, not tested yet).
-- Returns a journal or error message, and a flag indicating whether
-- it was re-read or not.  Like withJournalDo and journalReload, reads
-- the full journal, without filtering.
journalReloadIfChanged :: CliOpts -> Day -> Journal -> IO (Either String Journal, Bool)
journalReloadIfChanged opts _d j = do
  let maybeChangedFilename f = do newer <- journalSpecifiedFileIsNewer j f
                                  return $ if newer then Just f else Nothing
  changedfiles <- catMaybes `fmap` mapM maybeChangedFilename (journalFilePaths j)
  if not $ null changedfiles
   then do
     whenLoud $ printf "%s has changed, reloading\n" (head changedfiles)
     ej <- journalReload opts
     return (ej, True)
   else
     return (Right j, False)

-- | Has the journal's main data file changed since the journal was last
-- read ?
journalFileIsNewer :: Journal -> IO Bool
journalFileIsNewer j@Journal{jlastreadtime=tread} = do
  tmod <- fileModificationTime $ journalFilePath j
  return $ diffClockTimes tmod tread > (TimeDiff 0 0 0 0 0 0 0)

-- | Has the specified file (presumably one of journal's data files)
-- changed since journal was last read ?
journalSpecifiedFileIsNewer :: Journal -> FilePath -> IO Bool
journalSpecifiedFileIsNewer Journal{jlastreadtime=tread} f = do
  tmod <- fileModificationTime f
  return $ diffClockTimes tmod tread > (TimeDiff 0 0 0 0 0 0 0)

-- | Get the last modified time of the specified file, or if it does not
-- exist or there is some other error, the current time.
fileModificationTime :: FilePath -> IO ClockTime
fileModificationTime f
    | null f = getClockTime
    | otherwise = (do
        utc <- getModificationTime f
        let nom = utcTimeToPOSIXSeconds utc
        let clo = TOD (read $ takeWhile (`elem` "0123456789") $ show nom) 0 -- XXX read
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
writeFileWithBackupIfChanged :: FilePath -> T.Text -> IO Bool
writeFileWithBackupIfChanged f t = do
  s <- readFilePortably f
  if t == s then return False
            else backUpFile f >> T.writeFile f t >> return True

-- | Back up this file with a (incrementing) numbered suffix, then
-- overwrite it with this new text, or give an error.
writeFileWithBackup :: FilePath -> String -> IO ()
writeFileWithBackup f t = backUpFile f >> writeFile f t

readFileStrictly :: FilePath -> IO T.Text
readFileStrictly f = readFilePortably f >>= \s -> C.evaluate (T.length s) >> return s

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
