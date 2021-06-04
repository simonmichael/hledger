{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|

Utilities for top-level modules and ghci. See also Hledger.Read and
Hledger.Utils.

-}

module Hledger.Cli.Utils
    (
     unsupportedOutputFormatError,
     withJournalDo,
     writeOutput,
     writeOutputLazyText,
     journalTransform,
     journalAddForecast,
     journalReload,
     journalReloadIfChanged,
     journalFileIsNewer,
     openBrowserOn,
     writeFileWithBackup,
     writeFileWithBackupIfChanged,
     readFileStrictly,
     pivotByOpts,
     anonymiseByOpts,
     utcTimeToClockTime,
     journalSimilarTransaction,
     tests_Cli_Utils,
    )
where
import Control.Exception as C

import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time (UTCTime, Day, addDays)
import Safe (readMay, headMay)
import System.Console.CmdArgs
import System.Directory (getModificationTime, getDirectoryContents, copyFile, doesFileExist)
import System.Exit
import System.FilePath ((</>), splitFileName, takeDirectory)
import System.Info (os)
import System.Process (readProcessWithExitCode)
import System.Time (diffClockTimes, TimeDiff(TimeDiff))
import Text.Printf
import Text.Regex.TDFA ((=~))

import System.Time (ClockTime(TOD))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Hledger.Cli.CliOptions
import Hledger.Cli.Anon
import Hledger.Data
import Hledger.Read
import Hledger.Reports
import Hledger.Utils
import Control.Monad (when)

-- | Standard error message for a bad output format specified with -O/-o.
unsupportedOutputFormatError :: String -> String
unsupportedOutputFormatError fmt = "Sorry, output format \""++fmt++"\" is unrecognised or not yet supported for this kind of report."

-- | Parse the user's specified journal file(s) as a Journal, maybe apply some
-- transformations according to options, and run a hledger command with it.
-- Or, throw an error.
withJournalDo :: CliOpts -> (Journal -> IO a) -> IO a
withJournalDo opts cmd = do
  -- We kludgily read the file before parsing to grab the full text, unless
  -- it's stdin, or it doesn't exist and we are adding. We read it strictly
  -- to let the add command work.
  journalpaths <- journalFilePathFromOpts opts
  files <- readJournalFiles (inputopts_ opts) journalpaths
  let transformed = journalTransform opts <$> files
  either error' cmd transformed  -- PARTIAL:

-- | Apply some extra post-parse transformations to the journal, if
-- specified by options. These happen after journal validation, but
-- before report calculation. They include:
--
-- - adding forecast transactions (--forecast)
-- - pivoting account names (--pivot)
-- - anonymising (--anonymise).
--
journalTransform :: CliOpts -> Journal -> Journal
journalTransform opts =
    anonymiseByOpts opts
  -- - converting amounts to market value (--value)
  -- . journalApplyValue ropts
  . pivotByOpts opts
  . journalAddForecast opts

-- | Apply the pivot transformation on a journal, if option is present.
pivotByOpts :: CliOpts -> Journal -> Journal
pivotByOpts opts =
  case maybestringopt "pivot" . rawopts_ $ opts of
    Just tag -> journalPivot $ T.pack tag
    Nothing  -> id

-- | Apply the anonymisation transformation on a journal, if option is present
anonymiseByOpts :: CliOpts -> Journal -> Journal
anonymiseByOpts opts =
  if anon_ . inputopts_ $ opts
      then anon
      else id

-- | Generate periodic transactions from all periodic transaction rules in the journal.
-- These transactions are added to the in-memory Journal (but not the on-disk file).
--
-- When --auto is active, auto posting rules will be applied to the
-- generated transactions. If the query in any auto posting rule fails
-- to parse, this function will raise an error.
--
-- The start & end date for generated periodic transactions are determined in
-- a somewhat complicated way; see the hledger manual -> Periodic transactions.
--
journalAddForecast :: CliOpts -> Journal -> Journal
journalAddForecast CliOpts{inputopts_=iopts, reportspec_=rspec} j =
    case forecast_ ropts of
        Nothing -> j
        Just _  -> either (error') id . journalApplyCommodityStyles $  -- PARTIAL:
                     journalBalanceTransactions' iopts j{ jtxns = concat [jtxns j, forecasttxns'] }
  where
    today = rsToday rspec
    ropts = rsOpts rspec

    -- "They can start no earlier than: the day following the latest normal transaction in the journal (or today if there are none)."
    mjournalend   = dbg2 "journalEndDate" $ journalEndDate False j  -- ignore secondary dates
    forecastbeginDefault = dbg2 "forecastbeginDefault" $ fromMaybe today mjournalend

    -- "They end on or before the specified report end date, or 180 days from today if unspecified."
    mspecifiedend = dbg2 "specifieddates" $ reportPeriodLastDay rspec
    forecastendDefault = dbg2 "forecastendDefault" $ fromMaybe (addDays 180 today) mspecifiedend

    forecastspan = dbg2 "forecastspan" $
      spanDefaultsFrom
        (fromMaybe nulldatespan $ dbg2 "forecastspan flag" $ forecast_ ropts)
        (DateSpan (Just forecastbeginDefault) (Just forecastendDefault))

    forecasttxns =
      [ txnTieKnot t | pt <- jperiodictxns j
                     , t <- runPeriodicTransaction pt forecastspan
                     , spanContainsDate forecastspan (tdate t)
                     ]
    -- With --auto enabled, transaction modifiers are also applied to forecast txns
    forecasttxns' =
      (if auto_ iopts then either error' id . modifyTransactions today (jtxnmodifiers j) else id)  -- PARTIAL:
      forecasttxns

    journalBalanceTransactions' iopts j =
       either error' id $ journalBalanceTransactions (balancingopts_ iopts) j  -- PARTIAL:

-- | Write some output to stdout or to a file selected by --output-file.
-- If the file exists it will be overwritten.
writeOutput :: CliOpts -> String -> IO ()
writeOutput opts s = do
  f <- outputFileFromOpts opts
  (maybe putStr writeFile f) s

-- | Write some output to stdout or to a file selected by --output-file.
-- If the file exists it will be overwritten. This function operates on Lazy
-- Text values.
writeOutputLazyText :: CliOpts -> TL.Text -> IO ()
writeOutputLazyText opts s = do
  f <- outputFileFromOpts opts
  (maybe TL.putStr TL.writeFile f) s

-- -- | Get a journal from the given string and options, or throw an error.
-- readJournal :: CliOpts -> String -> IO Journal
-- readJournal opts s = readJournal def Nothing s >>= either error' return

-- | Re-read the option-specified journal file(s), but only if any of
-- them has changed since last read. (If the file is standard input,
-- this will either do nothing or give an error, not tested yet).
-- Returns a journal or error message, and a flag indicating whether
-- it was re-read or not.  Like withJournalDo and journalReload, reads
-- the full journal, without filtering.
journalReloadIfChanged :: CliOpts -> Day -> Journal -> IO (Either String Journal, Bool)
journalReloadIfChanged opts _d j = do
  let maybeChangedFilename f = do newer <- journalFileIsNewer j f
                                  return $ if newer then Just f else Nothing
  changedfiles <- catMaybes `fmap` mapM maybeChangedFilename (journalFilePaths j)
  if not $ null changedfiles
   then do
     -- XXX not sure why we use cmdarg's verbosity here, but keep it for now
     verbose <- isLoud
     when (verbose || debugLevel >= 6) $ printf "%s has changed, reloading\n" (head changedfiles)
     ej <- journalReload opts
     return (ej, True)
   else
     return (Right j, False)

-- | Re-read the journal file(s) specified by options, applying any
-- transformations specified by options. Or return an error string.
-- Reads the full journal, without filtering.
journalReload :: CliOpts -> IO (Either String Journal)
journalReload opts = do
  journalpaths <- dbg6 "reloading files" <$> journalFilePathFromOpts opts
  files <- readJournalFiles (inputopts_ opts) journalpaths
  return $ journalTransform opts <$> files

-- | Has the specified file changed since the journal was last read ?
-- Typically this is one of the journal's journalFilePaths. These are
-- not always real files, so the file's existence is tested first;
-- for non-files the answer is always no.
journalFileIsNewer :: Journal -> FilePath -> IO Bool
journalFileIsNewer Journal{jlastreadtime=tread} f = do
  mtmod <- maybeFileModificationTime f
  return $
    case mtmod of
      Just tmod -> diffClockTimes tmod tread > (TimeDiff 0 0 0 0 0 0 0)
      Nothing   -> False

-- | Get the last modified time of the specified file, if it exists.
maybeFileModificationTime :: FilePath -> IO (Maybe ClockTime)
maybeFileModificationTime f = do
  exists <- doesFileExist f
  if exists
  then do
    utc <- getModificationTime f
    return $ Just $ utcTimeToClockTime utc
  else
    return Nothing

utcTimeToClockTime :: UTCTime -> ClockTime
utcTimeToClockTime utc = TOD posixsecs picosecs
  where
    (posixsecs, frac) = properFraction $ utcTimeToPOSIXSeconds utc
    picosecs = round $ frac * 1e12

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
--
-- The given text should have unix line endings (\n); the existing
-- file content will be normalised to unix line endings before
-- comparing the two. If the file is overwritten, the new file will
-- have the current system's native line endings (\n on unix, \r\n on
-- windows). This could be different from the file's previous line
-- endings, if working with a DOS file on unix or vice-versa.
--
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

-- Identify the closest recent match for this description in past transactions.
-- If the options specify a query, only matched transactions are considered.
journalSimilarTransaction :: CliOpts -> Journal -> T.Text -> Maybe Transaction
journalSimilarTransaction cliopts j desc = mbestmatch
  where
    mbestmatch = snd <$> headMay bestmatches
    bestmatches =
      dbg1With (unlines . ("similar transactions:":) . map (\(score,Transaction{..}) -> printf "%0.3f %s %s" score (show tdate) tdescription)) $
      journalTransactionsSimilarTo j q desc 10
    q = queryFromFlags $ rsOpts $ reportspec_ cliopts

tests_Cli_Utils = tests "Utils" [

  --  tests "journalApplyValue" [
  --    -- Print the time required to convert one of the sample journals' amounts to value.
  --    -- Pretty clunky, but working.
  --    -- XXX sample.journal has no price records, but is always present.
  --    -- Change to eg examples/5000x1000x10.journal to make this useful.
  --    test "time" $ do
  --      ej <- io $ readJournalFile definputopts "examples/3000x1000x10.journal"
  --      case ej of
  --        Left e  -> crash $ T.pack e
  --        Right j -> do
  --          (t,_) <- io $ timeItT $ do
  --            -- Enable -V, and ensure the valuation date is later than
  --            -- all prices for consistent timing.
  --            let ropts = defreportopts{
  --              value_=True,
  --              period_=PeriodTo $ fromGregorian 3000 01 01
  --              }
  --            j' <- journalApplyValue ropts j
  --            sum (journalAmounts j') `seq` return ()
  --          io $ printf "[%.3fs] " t
  --          ok
  -- ]

 ]
