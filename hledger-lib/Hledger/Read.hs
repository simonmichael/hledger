--- * -*- outline-regexp:"--- \\*"; -*-
--- ** doc
-- In Emacs, use TAB on lines beginning with "-- *" to collapse/expand sections.
{-|

This is the entry point to hledger's reading system, which can read
Journals from various data formats. Use this module if you want to parse
journal data or read journal files. Generally it should not be necessary
to import modules below this one.

== Journal reading

There are three main Journal-reading functions:

- readJournal to read from a Text value.
  Identifies and calls an appropriate reader (parser + journalFinalise).
  The parser may call other parsers as needed to handle include directives,
  merging the resulting sub-Journals with the parent Journal as it goes.
  This overall Journal is finalised at the end.
  Then additional strict checking is done, if the inputopts specify it.

- readJournalFile to read one file, or stdin if the file path is @-@.
  Uses the file path/file name to help select the reader,
  and calls readJournal.

- readJournalFiles to read multiple files.
  Calls readJournalFile for each file,
  then merges all the Journals into one,
  then does strict checking if inputopts specify it.
  TODO: strict checking should be disabled until the end.

Each of these also has an easier variant with ' suffix,
which uses default options and has a simpler type signature.

One more variant, @readJournalFilesAndLatestDates@, is used by
the import command; it exposes the latest transaction date
(and how many on the same day) seen for each file,
after a successful import.

== Journal merging

Journal implements the Semigroup class, so two Journals can be merged
into one Journal with @j1 <> j2@. This is implemented by the
@journalConcat@ function, whose documentation explains what merging
Journals means exactly.

== Journal finalising

This is post-processing done after parsing an input file, such as
inferring missing information, normalising amount styles, doing extra
error checks, and so on - a delicate and influential stage of data
processing.

In hledger it is done by @journalFinalise@, which converts a
preliminary ParsedJournal to a validated, ready-to-use Journal.
This is called immediately after the parsing of each input file.
Notably, it is not called when Journals are merged.

-}

--- ** language
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

--- ** exports
module Hledger.Read (

  -- * Journal files
  PrefixedFilePath,
  defaultJournal,
  defaultJournalPath,
  requireJournalFileExists,
  ensureJournalFileExists,

  -- * Journal parsing
  runExceptT,
  readJournal,
  readJournalFile,
  readJournalFiles,
  readJournalFilesAndLatestDates,

  -- * Easy journal parsing
  readJournal',
  readJournalFile',
  readJournalFiles',
  orDieTrying,

  -- * Misc
  journalStrictChecks,
  saveLatestDates,

  -- * Re-exported
  JournalReader.tmpostingrulep,
  findReader,
  splitReaderPrefix,
  runJournalParser,
  module Hledger.Read.Common,
  module Hledger.Read.InputOptions,

  -- * Tests
  tests_Read,

) where

--- ** imports
import qualified Control.Exception as C
import Control.Monad (unless, when)
import "mtl" Control.Monad.Except (ExceptT(..), runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (def)
import Data.Foldable (asum)
import Data.List (group, sort, sortBy)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (Day)
import Safe (headDef)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.FilePath ((<.>), (</>), splitDirectories, splitFileName, takeFileName)
import System.Info (os)
import System.IO (hPutStr, stderr)

import Hledger.Data.Dates (getCurrentDay, parsedateM, showDate)
import Hledger.Data.Types
import Hledger.Read.Common
import Hledger.Read.InputOptions
import Hledger.Read.JournalReader as JournalReader
import Hledger.Read.CsvReader (tests_CsvReader)
import Hledger.Read.RulesReader (tests_RulesReader)
-- import Hledger.Read.TimedotReader (tests_TimedotReader)
-- import Hledger.Read.TimeclockReader (tests_TimeclockReader)
import Hledger.Utils
import Prelude hiding (getContents, writeFile)
import Hledger.Data.JournalChecks (journalCheckAccounts, journalCheckCommodities)

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

--- ** journal reading

journalEnvVar           = "LEDGER_FILE"
journalEnvVar2          = "LEDGER"
journalDefaultFilename  = ".hledger.journal"

-- | Read the default journal file specified by the environment, or raise an error.
defaultJournal :: IO Journal
defaultJournal = defaultJournalPath >>= runExceptT . readJournalFile definputopts >>= either error' return  -- PARTIAL:

-- | Get the default journal file path specified by the environment.
-- Like ledger, we look first for the LEDGER_FILE environment
-- variable, and if that does not exist, for the legacy LEDGER
-- environment variable. If neither is set, or the value is blank,
-- return the hard-coded default, which is @.hledger.journal@ in the
-- users's home directory (or in the current directory, if we cannot
-- determine a home directory).
defaultJournalPath :: IO String
defaultJournalPath = do
  s <- envJournalPath
  if null s then defpath else return s
    where
      envJournalPath =
        getEnv journalEnvVar
         `C.catch` (\(_::C.IOException) -> getEnv journalEnvVar2
                                            `C.catch` (\(_::C.IOException) -> return ""))
      defpath = do
        home <- getHomeDirectory `C.catch` (\(_::C.IOException) -> return "")
        return $ home </> journalDefaultFilename

-- | A file path optionally prefixed by a reader name and colon
-- (journal:, csv:, timedot:, etc.).
type PrefixedFilePath = FilePath

-- | @readJournal iopts mfile txt@
--
-- Read a Journal from some text, with strict checks if enabled,
-- or return an error message.
--
-- The reader (data format) is chosen based on, in this order:
--
-- - a reader name provided in @iopts@
--
-- - a reader prefix in the @mfile@ path
--
-- - a file extension in @mfile@
--
-- If none of these is available, or if the reader name is unrecognised,
-- we use the journal reader (for predictability).
--
readJournal :: InputOpts -> Maybe FilePath -> Text -> ExceptT String IO Journal
readJournal iopts@InputOpts{strict_} mpath txt = do
  let r :: Reader IO = fromMaybe JournalReader.reader $ findReader (mformat_ iopts) mpath
  dbg6IO "readJournal: trying reader" (rFormat r)
  j <- rReadFn r iopts (fromMaybe "(string)" mpath) txt
  when strict_ $ liftEither $ journalStrictChecks j
  return j

-- | Read a Journal from this file, or from stdin if the file path is -,
-- with strict checks if enabled, or return an error message.
-- The file path can have a READER: prefix.
--
-- The reader (data format) to use is determined from (in priority order):
-- the @mformat_@ specified in the input options, if any;
-- the file path's READER: prefix, if any;
-- a recognised file name extension.
-- if none of these identify a known reader, the journal reader is used.
--
-- The input options can also configure balance assertion checking, automated posting
-- generation, a rules file for converting CSV data, etc.
--
-- If using --new, and if latest-file writing is enabled in input options,
-- and after passing strict checks if enabled, a .latest.FILE file will be created/updated
-- (for the main file only, not for included files),
-- to remember the latest transaction date (and how many transactions on this date)
-- successfully read.
--
readJournalFile :: InputOpts -> PrefixedFilePath -> ExceptT String IO Journal
readJournalFile iopts@InputOpts{new_, new_save_} prefixedfile = do
  (j,latestdates) <- readJournalFileAndLatestDates iopts prefixedfile
  when (new_ && new_save_) $ liftIO $
    saveLatestDates latestdates (snd $ splitReaderPrefix prefixedfile)
  return j

-- The implementation of readJournalFile, but with --new, 
-- also returns the latest transaction date(s) read.
-- Used by readJournalFiles, to save those at the end.
readJournalFileAndLatestDates :: InputOpts -> PrefixedFilePath -> ExceptT String IO (Journal,LatestDates)
readJournalFileAndLatestDates iopts prefixedfile = do
  let
    (mfmt, f) = splitReaderPrefix prefixedfile
    iopts' = iopts{mformat_=asum [mfmt, mformat_ iopts]}
  liftIO $ requireJournalFileExists f
  t <-
    traceOrLogAt 6 ("readJournalFile: "++takeFileName f) $
    liftIO $ readFileOrStdinPortably f
    -- <- T.readFile f  -- or without line ending translation, for testing
  j <- readJournal iopts' (Just f) t
  if new_ iopts
    then do
      ds <- liftIO $ previousLatestDates f
      let (newj, newds) = journalFilterSinceLatestDates ds j
      return (newj, newds)
    else
      return (j, [])

-- | Read a Journal from each specified file path (using @readJournalFile@) 
-- and combine them into one; or return the first error message.
-- Strict checks, if enabled, are deferred till the end.
-- Writing .latest files, if enabled, is also deferred till the end,
-- and happens only if strict checks pass.
--
-- Combining Journals means concatenating them, basically.
-- The parse state resets at the start of each file, which means that
-- directives & aliases do not affect subsequent sibling or parent files.
-- They do affect included child files though.
-- Also the final parse state saved in the Journal does span all files.
--
readJournalFiles :: InputOpts -> [PrefixedFilePath] -> ExceptT String IO Journal
readJournalFiles iopts@InputOpts{strict_,new_,new_save_} prefixedfiles = do
  let iopts' = iopts{strict_=False, new_save_=False}
  (j,latestdates) <-
    traceOrLogAt 6 ("readJournalFiles: "++show prefixedfiles) $
    readJournalFilesAndLatestDates iopts' prefixedfiles
  when strict_ $ liftEither $ journalStrictChecks j
  when (new_ && new_save_) $ liftIO $
    mapM_ (saveLatestDates latestdates . snd . splitReaderPrefix) prefixedfiles
  return j

-- The implementation of readJournalFiles, but with --new, 
-- also returns the latest transaction date(s) read in each file.
-- Used by the import command, to save those at the end.
readJournalFilesAndLatestDates :: InputOpts -> [PrefixedFilePath] -> ExceptT String IO (Journal,LatestDates)
readJournalFilesAndLatestDates iopts =
  fmap (maybe def sconcat . nonEmpty) . mapM (readJournalFileAndLatestDates iopts)

-- | Run the extra -s/--strict checks on a journal,
-- returning the first error message if any of them fail.
journalStrictChecks :: Journal -> Either String ()
journalStrictChecks j = do
  journalCheckAccounts j
  journalCheckCommodities j

-- | An easy version of 'readJournal' which assumes default options, and fails
-- in the IO monad.
readJournal' :: Text -> IO Journal
readJournal' = orDieTrying . readJournal definputopts Nothing

-- | An easy version of 'readJournalFile' which assumes default options, and fails
-- in the IO monad.
readJournalFile' :: PrefixedFilePath -> IO Journal
readJournalFile' = orDieTrying . readJournalFile definputopts

-- | An easy version of 'readJournalFiles'' which assumes default options, and fails
-- in the IO monad.
readJournalFiles' :: [PrefixedFilePath] -> IO Journal
readJournalFiles' = orDieTrying . readJournalFiles definputopts

--- ** utilities

-- | Extract ExceptT to the IO monad, failing with an error message if necessary.
orDieTrying :: MonadIO m => ExceptT String m a -> m a
orDieTrying a = either (liftIO . fail) return =<< runExceptT a

-- | If the specified journal file does not exist (and is not "-"),
-- give a helpful error and quit.
requireJournalFileExists :: FilePath -> IO ()
requireJournalFileExists "-" = return ()
requireJournalFileExists f = do
  exists <- doesFileExist f
  unless exists $ do  -- XXX might not be a journal file
    hPutStr stderr $ "The hledger journal file \"" <> f <> "\" was not found.\n"
    hPutStr stderr "Please create it first, eg with \"hledger add\" or a text editor.\n"
    hPutStr stderr "Or, specify an existing journal file with -f or LEDGER_FILE.\n"
    exitFailure

-- | Ensure there is a journal file at the given path, creating an empty one if needed.
-- On Windows, also ensure that the path contains no trailing dots
-- which could cause data loss (see 'isWindowsUnsafeDotPath').
ensureJournalFileExists :: FilePath -> IO ()
ensureJournalFileExists f = do
  when (os/="mingw32" && isWindowsUnsafeDotPath f) $ do
    hPutStr stderr $ "Part of file path \"" <> show f <> "\"\n ends with a dot, which is unsafe on Windows; please use a different path.\n"
    exitFailure
  exists <- doesFileExist f
  unless exists $ do
    hPutStr stderr $ "Creating hledger journal file " <> show f <> ".\n"
    -- note Hledger.Utils.UTF8.* do no line ending conversion on windows,
    -- we currently require unix line endings on all platforms.
    newJournalContent >>= T.writeFile f

-- | Does any part of this path contain non-. characters and end with a . ?
-- Such paths are not safe to use on Windows (cf #1056).
isWindowsUnsafeDotPath :: FilePath -> Bool
isWindowsUnsafeDotPath = any (\x -> last x == '.' && any (/='.') x) . splitDirectories

-- | Give the content for a new auto-created journal file.
newJournalContent :: IO Text
newJournalContent = do
  d <- getCurrentDay
  return $ "; journal created " <> T.pack (show d) <> " by hledger\n"

-- A "LatestDates" is zero or more copies of the same date,
-- representing the latest transaction date read from a file,
-- and how many transactions there were on that date.
type LatestDates = [Day]

-- | Get all instances of the latest date in an unsorted list of dates.
-- Ie, if the latest date appears once, return it in a one-element list,
-- if it appears three times (anywhere), return three of it.
latestDates :: [Day] -> LatestDates
latestDates = {-# HLINT ignore "Avoid reverse" #-}
  headDef [] . take 1 . group . reverse . sort

-- | Save the given latest date(s) seen in the given data FILE,
-- in a hidden file named .latest.FILE, creating it if needed.
saveLatestDates :: LatestDates -> FilePath -> IO ()
saveLatestDates dates f = T.writeFile (latestDatesFileFor f) $ T.unlines $ map showDate dates

-- | What were the latest transaction dates seen the last time this
-- journal file was read ? If there were multiple transactions on the
-- latest date, that number of dates is returned, otherwise just one.
-- Or none if no transactions were read, or if latest dates info is not
-- available for this file.
previousLatestDates :: FilePath -> IO LatestDates
previousLatestDates f = do
  let latestfile = latestDatesFileFor f
      parsedate s = maybe (fail $ "could not parse date \"" ++ s ++ "\"") return $
                      parsedateM s
  exists <- doesFileExist latestfile
  if exists
  then traverse (parsedate . T.unpack . T.strip) . T.lines =<< readFileStrictly latestfile
  else return []

-- | Where to save latest transaction dates for the given file path.
-- (.latest.FILE)
latestDatesFileFor :: FilePath -> FilePath
latestDatesFileFor f = dir </> ".latest" <.> fname
  where
    (dir, fname) = splitFileName f

readFileStrictly :: FilePath -> IO Text
readFileStrictly f = readFilePortably f >>= \t -> C.evaluate (T.length t) >> return t

-- | Given zero or more latest dates (all the same, representing the
-- latest previously seen transaction date, and how many transactions
-- were seen on that date), remove transactions with earlier dates
-- from the journal, and the same number of transactions on the
-- latest date, if any, leaving only transactions that we can assume
-- are newer. Also returns the new latest dates of the new journal.
journalFilterSinceLatestDates :: LatestDates -> Journal -> (Journal, LatestDates)
journalFilterSinceLatestDates [] j       = (j,  latestDates $ map tdate $ jtxns j)
journalFilterSinceLatestDates ds@(d:_) j = (j', ds')
  where
    samedateorlaterts     = filter ((>= d).tdate) $ jtxns j
    (samedatets, laterts) = span ((== d).tdate) $ sortBy (comparing tdate) samedateorlaterts
    newsamedatets         = drop (length ds) samedatets
    j'                    = j{jtxns=newsamedatets++laterts}
    ds'                   = latestDates $ map tdate $ samedatets++laterts

--- ** tests

tests_Read = testGroup "Read" [
   tests_Common
  ,tests_CsvReader
  ,tests_JournalReader
  ,tests_RulesReader
  ]
