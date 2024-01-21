{-# LANGUAGE OverloadedStrings #-}
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
     journalReload,
     journalReloadIfChanged,
     journalFileIsNewer,
     openBrowserOn,
     writeFileWithBackup,
     writeFileWithBackupIfChanged,
     pivotByOpts,
     anonymiseByOpts,
     journalSimilarTransaction,
     postingsOrTransactionsReportAsText,
     tests_Cli_Utils,
    )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL
import Data.Time (Day)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Lens.Micro ((^.))
import Safe (readMay, headMay)
import System.Console.CmdArgs
import System.Directory (getModificationTime, getDirectoryContents, copyFile, doesFileExist)
import System.Exit
import System.FilePath ((</>), splitFileName, takeDirectory)
import System.Info (os)
import System.Process (readProcessWithExitCode)
import Text.Printf
import Text.Regex.TDFA ((=~))

import Hledger.Cli.CliOptions
import Hledger.Cli.Anon
import Hledger.Data
import Hledger.Read
import Hledger.Reports
import Hledger.Utils
import Control.Monad (when)
import Data.Functor ((<&>))

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
  j <- runExceptT $ journalTransform opts <$> readJournalFiles (inputopts_ opts) journalpaths
  either error' cmd j  -- PARTIAL:

-- | Apply some extra post-parse transformations to the journal, if enabled by options.
-- These happen after parsing and finalising the journal, but before report calculation.
-- They are, in processing order:
--
-- - pivoting account names (--pivot)
--
-- - anonymising (--anonymise).
--
journalTransform :: CliOpts -> Journal -> Journal
journalTransform opts =
      pivotByOpts opts
  <&> anonymiseByOpts opts

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
journalReloadIfChanged :: CliOpts -> Day -> Journal -> ExceptT String IO (Journal, Bool)
journalReloadIfChanged opts _d j = do
  let maybeChangedFilename f = do newer <- journalFileIsNewer j f
                                  return $ if newer then Just f else Nothing
  changedfiles <- liftIO $ catMaybes <$> mapM maybeChangedFilename (journalFilePaths j)
  if not $ null changedfiles
   then do
     -- XXX not sure why we use cmdarg's verbosity here, but keep it for now
     verbose <- liftIO isLoud
     when (verbose || debugLevel >= 6) . liftIO $ printf "%s has changed, reloading\n" (head changedfiles)
     newj <- journalReload opts
     return (newj, True)
   else
     return (j, False)

-- | Re-read the journal file(s) specified by options, applying any
-- transformations specified by options. Or return an error string.
-- Reads the full journal, without filtering.
journalReload :: CliOpts -> ExceptT String IO Journal
journalReload opts = do
  journalpaths <- liftIO $ dbg6 "reloading files" <$> journalFilePathFromOpts opts
  journalTransform opts <$> readJournalFiles (inputopts_ opts) journalpaths

-- | Has the specified file changed since the journal was last read ?
-- Typically this is one of the journal's journalFilePaths. These are
-- not always real files, so the file's existence is tested first;
-- for non-files the answer is always no.
journalFileIsNewer :: Journal -> FilePath -> IO Bool
journalFileIsNewer Journal{jlastreadtime=tread} f = do
  mtmod <- maybeFileModificationTime f
  return $
    case mtmod of
      Just tmod -> tmod > tread
      Nothing   -> False

-- | Get the last modified time of the specified file, if it exists.
maybeFileModificationTime :: FilePath -> IO (Maybe POSIXTime)
maybeFileModificationTime f = do
  exists <- doesFileExist f
  if exists
  then do
    utc <- getModificationTime f
    return . Just $ utcTimeToPOSIXSeconds utc
  else
    return Nothing

-- | Attempt to open a web browser on the given url, all platforms.
openBrowserOn :: String -> IO ExitCode
openBrowserOn = trybrowsers browsers
    where
      trybrowsers (b:bs) u1 = do
        (e,_,_) <- readProcessWithExitCode b [u1] ""
        case e of
          ExitSuccess -> return ExitSuccess
          ExitFailure _ -> trybrowsers bs u1
      trybrowsers [] u1 = do
        putStrLn $ printf "Could not start a web browser (tried: %s)" $ intercalate ", " browsers
        putStrLn $ printf "Please open your browser and visit %s" u1
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

-- | Back up this file with a (incrementing) numbered suffix, or give an error.
backUpFile :: FilePath -> IO ()
backUpFile fp = do
  fs <- safeGetDirectoryContents $ takeDirectory $ fp
  let (d,f) = splitFileName fp
      versions = mapMaybe (f `backupNumber`) fs
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
journalSimilarTransaction cliopts j desc =
  fmap fourth4 $ headMay $ journalTransactionsSimilarTo j desc q 0 1
  where
    q = queryFromFlags $ _rsReportOpts $ reportspec_ cliopts

-- | Render a 'PostingsReport' or 'AccountTransactionsReport' as Text,
-- determining the appropriate starting widths and increasing as necessary.
postingsOrTransactionsReportAsText
    :: Bool -> CliOpts -> (Int -> Int -> (a, [WideBuilder], [WideBuilder]) -> TB.Builder)
    -> (a -> MixedAmount) -> (a -> MixedAmount) -> [a] -> TB.Builder
postingsOrTransactionsReportAsText alignAll opts itemAsText itemamt itembal report =
    mconcat . snd $ mapAccumL renderItem (startWidth amt, startWidth bal) itemsWithAmounts
  where
    minWidth  = 12
    chunkSize = 1000

    renderItem (amtWidth, balWidth) item@(_, amt1, bal1) = ((amtWidth', balWidth'), itemBuilder)
      where
        itemBuilder = itemAsText amtWidth' balWidth' item
        amtWidth' = if alignAll then amtWidth else maximumStrict $ amtWidth : map wbWidth amt1
        balWidth' = if alignAll then balWidth else maximumStrict $ balWidth : map wbWidth bal1

    startWidth f = maximum $ minWidth : map wbWidth (concatMap f startAlign)
      where
        startAlign = (if alignAll then id else take chunkSize) itemsWithAmounts

    itemsWithAmounts = map (\x -> (x, showAmt $ itemamt x, showAmt $ itembal x)) report
    showAmt = showMixedAmountLinesB oneLine{displayColour=opts^.color__}
    amt = second3
    bal = third3

tests_Cli_Utils = testGroup "Utils" [

  --  testGroup "journalApplyValue" [
  --    -- Print the time required to convert one of the sample journals' amounts to value.
  --    -- Pretty clunky, but working.
  --    -- XXX sample.journal has no price records, but is always present.
  --    -- Change to eg examples/5000x1000x10.journal to make this useful.
  --    testCase "time" $ do
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
