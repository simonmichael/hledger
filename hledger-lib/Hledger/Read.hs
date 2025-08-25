--- * -*- outline-regexp:"--- \\*"; -*-
--- ** doc
-- In Emacs, use TAB on lines beginning with "-- *" to collapse/expand sections.
{-|

This is the entry point to hledger's reading system, which can read
Journals from various data formats. Use this module if you want to parse
journal data or read journal files. Generally it should not be necessary
to import modules below this one.

== Journal reading

Reading an input file (in journal, csv, timedot, or timeclock format..)
involves these steps:

- select an appropriate file format "reader"
  based on filename extension/file path prefix/function parameter.
  A reader contains a parser and a finaliser (usually @journalFinalise@).

- run the parser to get a ParsedJournal
  (this may run additional sub-parsers to parse included files)

- run the finaliser to get a complete Journal, which passes standard checks

- if reading multiple files: merge the per-file Journals into one
  overall Journal

- if using -s/--strict: run additional strict checks

- if running print --new: save .latest files for each input file.
  (import also does this, as its final step.)

== Journal merging

Journal implements the Semigroup class, so two Journals can be merged
into one Journal with @j1 <> j2@. This is implemented by the
@journalConcat@ function, whose documentation explains what merging
Journals means exactly.

== Journal finalising

This is post-processing done after parsing an input file, such as
inferring missing information, normalising amount styles,
checking for errors and so on - a delicate and influential stage
of data processing. 
In hledger it is done by @journalFinalise@, which converts a
preliminary ParsedJournal to a validated, ready-to-use Journal.
This is called immediately after the parsing of each input file.
It is not called when Journals are merged.

== Journal reading API

There are three main Journal-reading functions:

- readJournal to read from a Text value.
  Selects a reader and calls its parser and finaliser,
  then does strict checking if needed.

- readJournalFile to read one file, or stdin if the file path is @-@.
  Uses the file path/file name to help select the reader,
  calls readJournal,
  then writes .latest files if needed.

- readJournalFiles to read multiple files.
  Calls readJournalFile for each file (without strict checking or .latest file writing)
  then merges the Journals into one,
  then does strict checking and .latest file writing at the end if needed.

Each of these also has an easier variant with ' suffix,
which uses default options and has a simpler type signature.

One more variant, @readJournalFilesAndLatestDates@, is like
readJournalFiles but exposing the latest transaction date
(and how many on the same day) seen for each file.
This is used by the import command.

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
  defaultJournal,
  defaultJournalWith,
  defaultJournalSafely,
  defaultJournalSafelyWith,
  defaultJournalPath,
  requireJournalFileExists,
  ensureJournalFileExists,
  journalEnvVar,
  -- journalEnvVar2,
  journalDefaultFilename,

  -- * Journal parsing
  runExceptT,
  readJournal,
  readJournalFile,
  readJournalFiles,
  readJournalFilesAndLatestDates,

  -- * Easy journal parsing
  readJournal',
  readJournal'',
  readJournalFile',
  readJournalFiles',
  orDieTrying,

  -- * Misc
  saveLatestDates,
  saveLatestDatesForFiles,
  isWindowsUnsafeDotPath,

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
import Control.Monad (unless, when, forM, (<=<))
import "mtl" Control.Monad.Except (ExceptT(..), runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (def)
import Data.Foldable (asum)
import Data.List (group, sort, sortBy)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (Day)
import Safe (headDef, headMay)
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((<.>), (</>), splitDirectories, splitFileName, takeFileName)
import System.Info (os)
import System.IO (Handle, hPutStrLn, stderr)

import Hledger.Data.Dates (getCurrentDay, parsedate, showDate)
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
import Hledger.Data.JournalChecks (journalStrictChecks)
import Text.Printf (printf)

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

--- ** journal reading

journalEnvVar           = "LEDGER_FILE"
journalEnvVar2          = "LEDGER"
journalDefaultFilename  = ".hledger.journal"

-- | Read the default journal file specified by the environment, 
-- with default input options, or raise an error.
defaultJournal :: IO Journal
defaultJournal = defaultJournalSafely >>= either error' return -- PARTIAL:

-- | Read the default journal file specified by the environment,
-- with the given input options, or raise an error.
defaultJournalWith :: InputOpts -> IO Journal
defaultJournalWith iopts = defaultJournalSafelyWith iopts >>= either error' return -- PARTIAL:

-- | Read the default journal file specified by the environment,
-- with default input options, or return an error message.
defaultJournalSafely :: IO (Either String Journal)
defaultJournalSafely = defaultJournalSafelyWith definputopts

-- | Read the default journal file specified by the environment,
-- with the given input options, or return an error message.
defaultJournalSafelyWith :: InputOpts -> IO (Either String Journal)
defaultJournalSafelyWith iopts = (do
  f <- defaultJournalPath
  runExceptT $ readJournalFile iopts f
  ) `C.catches` [  -- XXX
     C.Handler (\(e :: C.ErrorCall)   -> return $ Left $ show e)
    ,C.Handler (\(e :: C.IOException) -> return $ Left $ show e)
    ]
-- | Get the default journal file path specified by the environment.
-- Like ledger, we look first for the LEDGER_FILE environment
-- variable, and if that does not exist, for the legacy LEDGER
-- environment variable. If neither is set, or the value is blank,
-- return the hard-coded default, which is @.hledger.journal@ in the
-- users's home directory (or in the current directory, if we cannot
-- determine a home directory).
defaultJournalPath :: IO String
defaultJournalPath = do
  p <- envJournalPath
  if null p
  then defpath
  else do
    ps <- expandGlob "." p `C.catch` (\(_::C.IOException) -> return [])
    maybe defpath return $ headMay ps
    where
      envJournalPath =
        getEnv journalEnvVar
         `C.catch` (\(_::C.IOException) -> getEnv journalEnvVar2
                                            `C.catch` (\(_::C.IOException) -> return ""))
      defpath = do
        home <- fromMaybe "" <$> getHomeSafe
        return $ home </> journalDefaultFilename

-- | @readJournal iopts mfile txt@
--
-- Read a Journal from some handle, with strict checks if enabled,
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
-- the journal reader is used.
--
-- If a file path is not provided, "-" is assumed (and may appear in error messages,
-- `files` output etc, where it will be a slight lie: it will mean "not from a file",
-- not necessarily "from standard input".
--
readJournal :: InputOpts -> Maybe FilePath -> Handle -> ExceptT String IO Journal
readJournal iopts@InputOpts{strict_, _defer} mpath hdl = do
  let r :: Reader IO = fromMaybe JournalReader.reader $ findReader (mformat_ iopts) mpath
  dbg6IO "readJournal: trying reader" (rFormat r)
  j <- rReadFn r iopts (fromMaybe "-" mpath) hdl
  when (strict_ && not _defer) $ liftEither $ journalStrictChecks j
  return j

-- | Read a Journal from this file, or from stdin if the file path is -,
-- with strict checks if enabled, or return an error message.
-- XXX or, calls error if the file does not exist.
--
-- (Note strict checks are disabled temporarily here when this is called by readJournalFiles).
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
-- and not deferred by readJournalFiles, and after passing strict checks if enabled,
-- a .latest.FILE file will be created/updated (for the main file only, not for included files),
-- to remember the latest transaction date processed.
--
readJournalFile :: InputOpts -> PrefixedFilePath -> ExceptT String IO Journal
readJournalFile iopts@InputOpts{new_, new_save_, _defer} prefixedfile = do
  (j, mlatestdates) <- readJournalFileAndLatestDates iopts prefixedfile
  when (new_ && new_save_ && not _defer) $ liftIO $
    case mlatestdates of
      Nothing                        -> return ()
      Just (LatestDatesForFile f ds) -> saveLatestDates ds f
  return j

-- The implementation of readJournalFile.
-- With --new, it also returns the latest transaction date(s) read from each file.
-- readJournalFiles uses this to update .latest files only after a successful read of all.
readJournalFileAndLatestDates :: InputOpts -> PrefixedFilePath -> ExceptT String IO (Journal, Maybe LatestDatesForFile)
readJournalFileAndLatestDates iopts prefixedfile = do
  let
    (mfmt, f) = splitReaderPrefix prefixedfile
    iopts' = iopts{mformat_=asum [mfmt, mformat_ iopts]}
  liftIO $ requireJournalFileExists f
  h <-
    dbg6Msg ("readJournalFile: "++takeFileName f) $
    liftIO $ openFileOrStdin f
    -- <- T.readFile f  -- or without line ending translation, for testing
  j <- readJournal iopts' (Just f) h
  if new_ iopts
    then do
      ds <- liftIO $ previousLatestDates f
      let (newj, newds) = journalFilterSinceLatestDates ds j
      return (newj, Just $ LatestDatesForFile f newds)
    else
      return (j, Nothing)

-- | Read a Journal from each specified file path (using @readJournalFile@) 
-- and combine them into one; or return the first error message.
--
-- Combining Journals means concatenating them, basically.
-- The parse state resets at the start of each file, which means that
-- directives & aliases do not affect subsequent sibling or parent files.
-- They do affect included child files though.
-- Also the final parse state saved in the Journal does span all files.
--
-- Strict checks, if enabled, are temporarily deferred until all files are read,
-- to ensure they see the whole journal, and/or to avoid redundant work.
-- (Some checks, like assertions and ordereddates, might still be doing redundant work ?)
--
-- Writing .latest files, if enabled, is also deferred till the end,
-- and is done only if strict checks pass.
--
readJournalFiles :: InputOpts -> [PrefixedFilePath] -> ExceptT String IO Journal
readJournalFiles iopts@InputOpts{strict_, new_, new_save_} prefixedfiles = do
  let iopts' = iopts{_defer=True}
  (j, latestdatesforfiles) <-
    dbg6Msg ("readJournalFiles: "++show prefixedfiles) $
    readJournalFilesAndLatestDates iopts' prefixedfiles
  when strict_ $ liftEither $ journalStrictChecks j
  when (new_ && new_save_) $ liftIO $ saveLatestDatesForFiles latestdatesforfiles
  return j

-- The implementation of readJournalFiles.
-- With --new, it also returns the latest transaction date(s) read in each file
-- (used by the import command).
readJournalFilesAndLatestDates :: InputOpts -> [PrefixedFilePath] -> ExceptT String IO (Journal, [LatestDatesForFile])
readJournalFilesAndLatestDates iopts pfs = do
  (js, lastdates) <- unzip <$> mapM (readJournalFileAndLatestDates iopts) pfs
  return (maybe def sconcat $ nonEmpty js, catMaybes lastdates)

-- | An easy version of 'readJournal' which assumes default options, and fails in the IO monad.
readJournal' :: Handle -> IO Journal
readJournal' = orDieTrying . readJournal definputopts Nothing

-- | An even easier version of readJournal' which takes a 'Text' instead of a 'Handle'.
readJournal'' :: Text -> IO Journal
readJournal'' = readJournal' <=< inputToHandle

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

-- | If the specified journal file does not exist (and is not "-"), call error with an informative message.
-- (Using "journal file" generically here; it could be in any of hledger's supported formats.)
requireJournalFileExists :: FilePath -> IO ()
requireJournalFileExists "-" = return ()
requireJournalFileExists f = do
  exists <- doesFileExist f
  unless exists $ error' $ unlines
    [ "data file \"" <> f <> "\" was not found."
    ,"Please create it first, eg with \"hledger add\" or a text editor."
    ,"Or, specify an existing data file with -f or $LEDGER_FILE."
    ]

-- | Ensure there is a journal file at the given path, creating an empty one if needed.
-- On Windows, also ensure that the path contains no trailing dots
-- which could cause data loss (see 'isWindowsUnsafeDotPath').
ensureJournalFileExists :: FilePath -> IO ()
ensureJournalFileExists f = do
  when (os=="mingw32" && isWindowsUnsafeDotPath f) $
    error' $ "Part of file path \"" <> show f <> "\"\n ends with a dot, which is unsafe on Windows; please use a different path.\n"
  exists <- doesFileExist f
  unless exists $ do
    hPutStrLn stderr $ "Creating hledger journal file " <> show f
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

-- The path of an input file, and its current "LatestDates".
data LatestDatesForFile = LatestDatesForFile FilePath LatestDates
  deriving Show

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

-- | Save each file's latest dates.
saveLatestDatesForFiles :: [LatestDatesForFile] -> IO ()
saveLatestDatesForFiles = mapM_ (\(LatestDatesForFile f ds) -> saveLatestDates ds f)

-- | What were the latest transaction dates seen the last time this
-- journal file was read ? If there were multiple transactions on the
-- latest date, that number of dates is returned, otherwise just one.
-- Or none if no transactions were read, or if latest dates info is not
-- available for this file.
previousLatestDates :: FilePath -> IO LatestDates
previousLatestDates f = do
  let latestfile = latestDatesFileFor f
  exists <- doesFileExist latestfile
  t <- if exists then readFileStrictly latestfile else return T.empty
  let nls = zip [1::Int ..] $ T.lines t
  fmap catMaybes $ forM nls $ \(n,l) -> do
    let s = T.unpack $ T.strip l
    case (s, parsedate s) of
      ("", _)       -> return Nothing
      (_,  Nothing) -> error' (printf "%s:%d: invalid date: \"%s\"" latestfile n s)
      (_,  Just d)  -> return $ Just d

-- | Where to save latest transaction dates for the given file path.
-- (.latest.FILE)
latestDatesFileFor :: FilePath -> FilePath
latestDatesFileFor f = dir </> ".latest" <.> fname
  where
    (dir, fname) = splitFileName f

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
