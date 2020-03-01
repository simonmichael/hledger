-- * -*- eval: (orgstruct-mode 1); orgstruct-heading-prefix-regexp:"-- "; -*-
-- ** doc
-- In Emacs, use TAB on lines beginning with "-- *" to collapse/expand sections.
{-|

This is the entry point to hledger's reading system, which can read
Journals from various data formats. Use this module if you want to parse
journal data or read journal files. Generally it should not be necessary
to import modules below this one.

-}

-- ** language
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

-- ** exports
module Hledger.Read (

  -- * Journal files
  PrefixedFilePath,
  defaultJournal,
  defaultJournalPath,
  readJournalFiles,
  readJournalFile,
  requireJournalFileExists,
  ensureJournalFileExists,
  splitReaderPrefix,

  -- * Journal parsing
  readJournal,
  readJournal',

  -- * Re-exported
  JournalReader.accountaliasp,
  JournalReader.postingp,
  module Hledger.Read.Common,

  -- * Tests
  tests_Read,

) where

-- ** imports
import Control.Arrow (right)
import qualified Control.Exception as C
import Control.Monad (when)
import "mtl" Control.Monad.Except (runExceptT)
import Data.Default
import Data.List
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Safe
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.FilePath
import System.Info (os)
import System.IO
import Text.Printf

import Hledger.Data.Dates (getCurrentDay, parsedate, showDate)
import Hledger.Data.Types
import Hledger.Read.Common
import Hledger.Read.JournalReader   as JournalReader
import qualified Hledger.Read.TimedotReader   as TimedotReader
import qualified Hledger.Read.TimeclockReader as TimeclockReader
import Hledger.Read.CsvReader as CsvReader
import Hledger.Utils
import Prelude hiding (getContents, writeFile)

-- ** environment

journalEnvVar           = "LEDGER_FILE"
journalEnvVar2          = "LEDGER"
journalDefaultFilename  = ".hledger.journal"

-- ** journal reading

-- The available journal readers, each one handling a particular data format.
readers :: [Reader]
readers = [
  JournalReader.reader
 ,TimeclockReader.reader
 ,TimedotReader.reader
 ,CsvReader.reader
--  ,LedgerReader.reader
 ]

readerNames :: [String]
readerNames = map rFormat readers

-- | Read a Journal from the given text, assuming journal format; or
-- throw an error.
readJournal' :: Text -> IO Journal
readJournal' t = readJournal def Nothing t >>= either error' return

-- | @readJournal iopts mfile txt@
--
-- Read a Journal from some text, or return an error message.
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
-- we use the journal reader. (We used to try all readers in this case;
-- since hledger 1.17, we prefer predictability.)
readJournal :: InputOpts -> Maybe FilePath -> Text -> IO (Either String Journal)
readJournal iopts mpath txt = do
  dbg1IO "trying reader" (rFormat r)
  ej <- (runExceptT . (rParser r) iopts (fromMaybe "(string)" mpath)) txt
  dbg1IO "reader result" (' ':show ej)
  return ej
  where
    r = fromMaybe JournalReader.reader $ findReader (mformat_ iopts) mpath

-- | @findReader mformat mpath@
--
-- Find the reader named by @mformat@, if provided.
-- Or, if a file path is provided, find the first reader that handles
-- its file extension, if any.
findReader :: Maybe StorageFormat -> Maybe FilePath -> Maybe Reader
findReader Nothing Nothing     = Nothing
findReader (Just fmt) _        = headMay [r | r <- readers, rFormat r == fmt]
findReader Nothing (Just path) =
  case prefix of
    Just fmt -> headMay [r | r <- readers, rFormat r == fmt]
    Nothing  -> headMay [r | r <- readers, ext `elem` rExtensions r]
  where
    (prefix,path') = splitReaderPrefix path
    ext            = drop 1 $ takeExtension path'

-- | Read the default journal file specified by the environment, or raise an error.
defaultJournal :: IO Journal
defaultJournal = defaultJournalPath >>= readJournalFile def >>= either error' return

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
  if null s then defaultJournalPath else return s
    where
      envJournalPath =
        getEnv journalEnvVar
         `C.catch` (\(_::C.IOException) -> getEnv journalEnvVar2
                                            `C.catch` (\(_::C.IOException) -> return ""))
      defaultJournalPath = do
                  home <- getHomeDirectory `C.catch` (\(_::C.IOException) -> return "")
                  return $ home </> journalDefaultFilename

-- | A file path optionally prefixed by a reader name and colon
-- (journal:, csv:, timedot:, etc.).
type PrefixedFilePath = FilePath

-- | Read a Journal from each specified file path and combine them into one.
-- Or, return the first error message.
--
-- Combining Journals means concatenating them, basically.
-- The parse state resets at the start of each file, which means that
-- directives & aliases do not affect subsequent sibling or parent files.
-- They do affect included child files though.
-- Also the final parse state saved in the Journal does span all files.
readJournalFiles :: InputOpts -> [PrefixedFilePath] -> IO (Either String Journal)
readJournalFiles iopts =
  (right mconcat1 . sequence <$>) . mapM (readJournalFile iopts)
  where
    mconcat1 :: Monoid t => [t] -> t
    mconcat1 [] = mempty
    mconcat1 x  = foldr1 mappend x

-- | Read a Journal from this file, or from stdin if the file path is -,
-- or return an error message. The file path can have a READER: prefix.
--
-- The reader (data format) to use is determined from (in priority order):
-- the @mformat_@ specified in the input options, if any;
-- the file path's READER: prefix, if any;
-- a recognised file name extension.
-- if none of these identify a known reader, the journal reader is used.
--
-- The input options can also configure balance assertion checking, automated posting
-- generation, a rules file for converting CSV data, etc.
readJournalFile :: InputOpts -> PrefixedFilePath -> IO (Either String Journal)
readJournalFile iopts prefixedfile = do
  let
    (mfmt, f) = splitReaderPrefix prefixedfile
    iopts' = iopts{mformat_=firstJust [mfmt, mformat_ iopts]}
  requireJournalFileExists f
  t <- readFileOrStdinPortably f
    -- <- T.readFile f  -- or without line ending translation, for testing
  ej <- readJournal iopts' (Just f) t
  case ej of
    Left e  -> return $ Left e
    Right j | new_ iopts -> do
      ds <- previousLatestDates f
      let (newj, newds) = journalFilterSinceLatestDates ds j
      when (new_save_ iopts && not (null newds)) $ saveLatestDates newds f
      return $ Right newj
    Right j -> return $ Right j

-- ** utilities

-- | If a filepath is prefixed by one of the reader names and a colon,
-- split that off. Eg "csv:-" -> (Just "csv", "-").
splitReaderPrefix :: PrefixedFilePath -> (Maybe String, FilePath)
splitReaderPrefix f =
  headDef (Nothing, f)
  [(Just r, drop (length r + 1) f) | r <- readerNames, (r++":") `isPrefixOf` f]

-- | If the specified journal file does not exist (and is not "-"),
-- give a helpful error and quit.
requireJournalFileExists :: FilePath -> IO ()
requireJournalFileExists "-" = return ()
requireJournalFileExists f = do
  exists <- doesFileExist f
  when (not exists) $ do  -- XXX might not be a journal file
    hPrintf stderr "The hledger journal file \"%s\" was not found.\n" f
    hPrintf stderr "Please create it first, eg with \"hledger add\" or a text editor.\n"
    hPrintf stderr "Or, specify an existing journal file with -f or LEDGER_FILE.\n"
    exitFailure

-- | Ensure there is a journal file at the given path, creating an empty one if needed.
-- On Windows, also ensure that the path contains no trailing dots
-- which could cause data loss (see 'isWindowsUnsafeDotPath').
ensureJournalFileExists :: FilePath -> IO ()
ensureJournalFileExists f = do
  when (os/="mingw32" && isWindowsUnsafeDotPath f) $ do
    hPrintf stderr "Part of file path %s\n ends with a dot, which is unsafe on Windows; please use a different path.\n" (show f)
    exitFailure
  exists <- doesFileExist f
  when (not exists) $ do
    hPrintf stderr "Creating hledger journal file %s.\n" f
    -- note Hledger.Utils.UTF8.* do no line ending conversion on windows,
    -- we currently require unix line endings on all platforms.
    newJournalContent >>= writeFile f

-- | Does any part of this path contain non-. characters and end with a . ?
-- Such paths are not safe to use on Windows (cf #1056).
isWindowsUnsafeDotPath :: FilePath -> Bool
isWindowsUnsafeDotPath =
  not . null .
  filter (not . all (=='.')) .
  filter ((=='.').last) .
  splitDirectories

-- | Give the content for a new auto-created journal file.
newJournalContent :: IO String
newJournalContent = do
  d <- getCurrentDay
  return $ printf "; journal created %s by hledger\n" (show d)

-- A "LatestDates" is zero or more copies of the same date,
-- representing the latest transaction date read from a file,
-- and how many transactions there were on that date.
type LatestDates = [Day]

-- | Get all instances of the latest date in an unsorted list of dates.
-- Ie, if the latest date appears once, return it in a one-element list,
-- if it appears three times (anywhere), return three of it.
latestDates :: [Day] -> LatestDates
latestDates = headDef [] . take 1 . group . reverse . sort

-- | Remember that these transaction dates were the latest seen when
-- reading this journal file.
saveLatestDates :: LatestDates -> FilePath -> IO ()
saveLatestDates dates f = writeFile (latestDatesFileFor f) $ unlines $ map showDate dates

-- | What were the latest transaction dates seen the last time this
-- journal file was read ? If there were multiple transactions on the
-- latest date, that number of dates is returned, otherwise just one.
-- Or none if no transactions were read, or if latest dates info is not
-- available for this file.
previousLatestDates :: FilePath -> IO LatestDates
previousLatestDates f = do
  let latestfile = latestDatesFileFor f
  exists <- doesFileExist latestfile
  if exists
  then map (parsedate . strip) . lines . strip . T.unpack <$> readFileStrictly latestfile
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

-- ** tests

tests_Read = tests "Read" [
   tests_Common
  ,tests_CsvReader
  ,tests_JournalReader
  ]
