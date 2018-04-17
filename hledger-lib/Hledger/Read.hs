{-|

This is the entry point to hledger's reading system, which can read
Journals from various data formats. Use this module if you want to parse
journal data or read journal files. Generally it should not be necessary
to import modules below this one.
-}

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Hledger.Read (

  -- * Journal files
  PrefixedFilePath,
  defaultJournal,
  defaultJournalPath,
  readJournalFilesWithOpts,
  readJournalFileWithOpts,
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
  samplejournal,
  tests_Hledger_Read,

) where

import Control.Arrow (right)
import qualified Control.Exception as C
import Control.Monad.Except
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
import System.IO
import Test.HUnit
import Text.Printf

import Hledger.Data.Dates (getCurrentDay, parsedate, showDate)
import Hledger.Data.Types
import Hledger.Read.Common
import qualified Hledger.Read.JournalReader   as JournalReader
-- import qualified Hledger.Read.LedgerReader    as LedgerReader
import qualified Hledger.Read.TimedotReader   as TimedotReader
import qualified Hledger.Read.TimeclockReader as TimeclockReader
import qualified Hledger.Read.CsvReader       as CsvReader
import Hledger.Utils
import Prelude hiding (getContents, writeFile)


journalEnvVar           = "LEDGER_FILE"
journalEnvVar2          = "LEDGER"
journalDefaultFilename  = ".hledger.journal"

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

-- | A file path optionally prefixed by a reader name and colon
-- (journal:, csv:, timedot:, etc.).
type PrefixedFilePath = FilePath

-- | Read the default journal file specified by the environment, or raise an error.
defaultJournal :: IO Journal
defaultJournal = defaultJournalPath >>= readJournalFileWithOpts def >>= either error' return

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
ensureJournalFileExists :: FilePath -> IO ()
ensureJournalFileExists f = do
  exists <- doesFileExist f
  when (not exists) $ do
    hPrintf stderr "Creating hledger journal file %s.\n" f
    -- note Hledger.Utils.UTF8.* do no line ending conversion on windows,
    -- we currently require unix line endings on all platforms.
    newJournalContent >>= writeFile f

-- | Give the content for a new auto-created journal file.
newJournalContent :: IO String
newJournalContent = do
  d <- getCurrentDay
  return $ printf "; journal created %s by hledger\n" (show d)

-- | Read a Journal from the given text trying all readers in turn, or throw an error.
readJournal' :: Text -> IO Journal
readJournal' t = readJournal Nothing def Nothing t >>= either error' return

tests_readJournal' = [
  "readJournal' parses sample journal" ~: do
     _ <- samplejournal
     assertBool "" True
 ]

-- | @readJournal mformat mrulesfile assrt mfile txt@
--
-- Read a Journal from some text, or return an error message.
--
-- The reader (data format) is chosen based on (in priority order):
-- the @mformat@ argument;
-- a recognised file name extension in @mfile@ (if provided).
-- If none of these identify a known reader, all built-in readers are tried in turn
-- (returning the first one's error message if none of them succeed).
--
-- Input ioptions (@iopts@) specify CSV conversion rules file to help convert CSV data,
-- enable or disable balance assertion checking and automated posting generation.
--
readJournal :: Maybe StorageFormat -> InputOpts -> Maybe FilePath -> Text -> IO (Either String Journal)
readJournal mformat iopts mfile txt =
  let
    stablereaders = filter (not.rExperimental) readers
    rs = maybe stablereaders (:[]) $ findReader mformat mfile
  in
    tryReaders rs iopts mfile txt

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

-- | @tryReaders readers mrulesfile assrt path t@
--
-- Try to parse the given text to a Journal using each reader in turn,
-- returning the first success, or if all of them fail, the first error message.
tryReaders :: [Reader] -> InputOpts -> Maybe FilePath -> Text -> IO (Either String Journal)
tryReaders readers iopts path t = firstSuccessOrFirstError [] readers
  where
    firstSuccessOrFirstError :: [String] -> [Reader] -> IO (Either String Journal)
    firstSuccessOrFirstError [] []        = return $ Left "no readers found"
    firstSuccessOrFirstError errs (r:rs) = do
      dbg1IO "trying reader" (rFormat r)
      result <- (runExceptT . (rParser r) iopts path') t
      dbg1IO "reader result" $ either id show result
      case result of Right j -> return $ Right j                        -- success!
                     Left e  -> firstSuccessOrFirstError (errs++[e]) rs -- keep trying
    firstSuccessOrFirstError (e:_) []    = return $ Left e              -- none left, return first error
    path' = fromMaybe "(string)" path


--- New versions of readJournal* with easier arguments, and support for --new.

readJournalFilesWithOpts :: InputOpts -> [FilePath] -> IO (Either String Journal)
readJournalFilesWithOpts iopts =
  (right mconcat1 . sequence <$>) . mapM (readJournalFileWithOpts iopts)
  where
    mconcat1 :: Monoid t => [t] -> t
    mconcat1 [] = mempty
    mconcat1 x  = foldr1 mappend x

readJournalFileWithOpts :: InputOpts -> PrefixedFilePath -> IO (Either String Journal)
readJournalFileWithOpts iopts prefixedfile = do
  let 
    (mfmt, f) = splitReaderPrefix prefixedfile
    iopts' = iopts{mformat_=firstJust [mfmt, mformat_ iopts]}
  requireJournalFileExists f
  t <- readFileOrStdinPortably f
  ej <- readJournalWithOpts iopts' (Just f) t
  case ej of
    Left e  -> return $ Left e
    Right j | new_ iopts -> do
      ds <- previousLatestDates f
      let (newj, newds) = journalFilterSinceLatestDates ds j
      when (new_save_ iopts && not (null newds)) $ saveLatestDates newds f
      return $ Right newj
    Right j -> return $ Right j

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

readJournalWithOpts :: InputOpts -> Maybe FilePath -> Text -> IO (Either String Journal)
readJournalWithOpts iopts mfile txt =
  tryReadersWithOpts iopts mfile specifiedorallreaders txt
  where
    specifiedorallreaders = maybe stablereaders (:[]) $ findReader (mformat_ iopts) mfile
    stablereaders = filter (not.rExperimental) readers

tryReadersWithOpts :: InputOpts -> Maybe FilePath -> [Reader] -> Text -> IO (Either String Journal)
tryReadersWithOpts iopts mpath readers txt = firstSuccessOrFirstError [] readers
  where
    firstSuccessOrFirstError :: [String] -> [Reader] -> IO (Either String Journal)
    firstSuccessOrFirstError [] []        = return $ Left "no readers found"
    firstSuccessOrFirstError errs (r:rs) = do
      dbg1IO "trying reader" (rFormat r)
      result <- (runExceptT . (rParser r) iopts path) txt
      dbg1IO "reader result" $ either id show result
      case result of Right j -> return $ Right j                        -- success!
                     Left e  -> firstSuccessOrFirstError (errs++[e]) rs -- keep trying
    firstSuccessOrFirstError (e:_) []    = return $ Left e              -- none left, return first error
    path = fromMaybe "(string)" mpath

---


-- tests

samplejournal = readJournal' $ T.unlines
 ["2008/01/01 income"
 ,"    assets:bank:checking  $1"
 ,"    income:salary"
 ,""
 ,"comment"
 ,"multi line comment here"
 ,"for testing purposes"
 ,"end comment"
 ,""
 ,"2008/06/01 gift"
 ,"    assets:bank:checking  $1"
 ,"    income:gifts"
 ,""
 ,"2008/06/02 save"
 ,"    assets:bank:saving  $1"
 ,"    assets:bank:checking"
 ,""
 ,"2008/06/03 * eat & shop"
 ,"    expenses:food      $1"
 ,"    expenses:supplies  $1"
 ,"    assets:cash"
 ,""
 ,"2008/12/31 * pay off"
 ,"    liabilities:debts  $1"
 ,"    assets:bank:checking"
 ]

tests_Hledger_Read = TestList $
  tests_readJournal'
  ++ [
   JournalReader.tests_Hledger_Read_JournalReader,
--    LedgerReader.tests_Hledger_Read_LedgerReader,
   TimeclockReader.tests_Hledger_Read_TimeclockReader,
   TimedotReader.tests_Hledger_Read_TimedotReader,
   CsvReader.tests_Hledger_Read_CsvReader,

   "journal" ~: do
    r <- runExceptT $ parseWithState mempty JournalReader.journalp ""
    assertBool "journalp should parse an empty file" (isRight $ r)
    jE <- readJournal Nothing def Nothing "" -- don't know how to get it from journal
    either error' (assertBool "journalp parsing an empty file should give an empty journal" . null . jtxns) jE

  ]
