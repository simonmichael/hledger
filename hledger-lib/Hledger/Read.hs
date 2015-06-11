{-# LANGUAGE ScopedTypeVariables #-}
{-|

This is the entry point to hledger's reading system, which can read
Journals from various data formats. Use this module if you want to parse
journal data or read journal files. Generally it should not be necessary
to import modules below this one.

-}

module Hledger.Read (
       -- * Journal reading API
       defaultJournalPath,
       defaultJournal,
       readJournal,
       readJournal',
       readJournalFile,
       readJournalFiles,
       requireJournalFileExists,
       ensureJournalFileExists,
       -- * Parsers used elsewhere
       postingp,
       accountnamep,
       amountp,
       amountp',
       mamountp',
       numberp,
       codep,
       accountaliasp,
       -- * Tests
       samplejournal,
       tests_Hledger_Read,
)
where
import qualified Control.Exception as C
import Control.Monad.Except
import Data.List
import Data.Maybe
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode(..), openFile, stdin, stderr, hSetNewlineMode, universalNewlineMode)
import Test.HUnit
import Text.Printf

import Hledger.Data.Dates (getCurrentDay)
import Hledger.Data.Types
import Hledger.Data.Journal (nullctx)
import Hledger.Read.JournalReader as JournalReader
import Hledger.Read.TimelogReader as TimelogReader
import Hledger.Read.CsvReader as CsvReader
import Hledger.Utils
import Prelude hiding (getContents, writeFile)
import Hledger.Utils.UTF8IOCompat (hGetContents, writeFile)


journalEnvVar           = "LEDGER_FILE"
journalEnvVar2          = "LEDGER"
journalDefaultFilename  = ".hledger.journal"

-- The available data file readers, each one handling a particular data
-- format. The first is also used as the default for unknown formats.
readers :: [Reader]
readers = [
  JournalReader.reader
 ,TimelogReader.reader
 ,CsvReader.reader
 ]

-- | All the data formats we can read.
-- formats = map rFormat readers

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

-- | Read the default journal file specified by the environment, or raise an error.
defaultJournal :: IO Journal
defaultJournal = defaultJournalPath >>= readJournalFile Nothing Nothing True >>= either error' return

-- | Read a journal from the given string, trying all known formats, or simply throw an error.
readJournal' :: String -> IO Journal
readJournal' s = readJournal Nothing Nothing True Nothing s >>= either error' return

tests_readJournal' = [
  "readJournal' parses sample journal" ~: do
     _ <- samplejournal
     assertBool "" True
 ]



-- | Read a journal from this string, trying whatever readers seem appropriate:
--
-- - if a format is specified, try that reader only
--
-- - or if one or more readers recognises the file path and data, try those
--
-- - otherwise, try them all.
--
-- A CSV conversion rules file may also be specified for use by the CSV reader.
-- Also there is a flag specifying whether to check or ignore balance assertions in the journal.
readJournal :: Maybe StorageFormat -> Maybe FilePath -> Bool -> Maybe FilePath -> String -> IO (Either String Journal)
readJournal format rulesfile assrt path s =
  tryReaders $ readersFor (format, path, s)
  where
    -- try each reader in turn, returning the error of the first if all fail
    tryReaders :: [Reader] -> IO (Either String Journal)
    tryReaders = firstSuccessOrBestError []
      where
        firstSuccessOrBestError :: [String] -> [Reader] -> IO (Either String Journal)
        firstSuccessOrBestError [] []        = return $ Left "no readers found"
        firstSuccessOrBestError errs (r:rs) = do
          dbg1IO "trying reader" (rFormat r)
          result <- (runExceptT . (rParser r) rulesfile assrt path') s
          dbg1IO "reader result" $ either id show result
          case result of Right j -> return $ Right j                       -- success!
                         Left e  -> firstSuccessOrBestError (errs++[e]) rs -- keep trying
        firstSuccessOrBestError (e:_) []    = return $ Left e              -- none left, return first error
        path' = fromMaybe "(string)" path

-- | Which readers are worth trying for this (possibly unspecified) format, filepath, and data ?
readersFor :: (Maybe StorageFormat, Maybe FilePath, String) -> [Reader]
readersFor (format,path,s) =
    dbg1 ("possible readers for "++show (format,path,elideRight 30 s)) $
    case format of
     Just f  -> case readerForStorageFormat f of Just r  -> [r]
                                                 Nothing -> []
     Nothing -> case path of Nothing  -> readers
                             Just p   -> case readersForPathAndData (p,s) of [] -> readers
                                                                             rs -> rs

-- | Find the (first) reader which can handle the given format, if any.
readerForStorageFormat :: StorageFormat -> Maybe Reader
readerForStorageFormat s | null rs = Nothing
                  | otherwise = Just $ head rs
    where
      rs = filter ((s==).rFormat) readers :: [Reader]

-- | Find the readers which think they can handle the given file path and data, if any.
readersForPathAndData :: (FilePath,String) -> [Reader]
readersForPathAndData (f,s) = filter (\r -> (rDetector r) f s) readers

-- | Read a Journal from this file (or stdin if the filename is -) or give
-- an error message, using the specified data format or trying all known
-- formats. A CSV conversion rules file may be specified for better
-- conversion of that format. Also there is a flag specifying whether
-- to check or ignore balance assertions in the journal.
readJournalFile :: Maybe StorageFormat -> Maybe FilePath -> Bool -> FilePath -> IO (Either String Journal)
readJournalFile format rulesfile assrt f = readJournalFiles format rulesfile assrt [f]

readJournalFiles :: Maybe StorageFormat -> Maybe FilePath -> Bool -> [FilePath] -> IO (Either String Journal)
readJournalFiles format rulesfile assrt f = do
  contents <- fmap concat $ mapM readFileAnyNewline f
  readJournal format rulesfile assrt (listToMaybe f) contents
 where
  readFileAnyNewline f = do
    requireJournalFileExists f
    h <- fileHandle f
    hSetNewlineMode h universalNewlineMode
    hGetContents h
  fileHandle "-" = return stdin
  fileHandle f = openFile f ReadMode

-- | If the specified journal file does not exist, give a helpful error and quit.
requireJournalFileExists :: FilePath -> IO ()
requireJournalFileExists "-" = return ()
requireJournalFileExists f = do
  exists <- doesFileExist f
  when (not exists) $ do
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

-- tests

samplejournal = readJournal' $ unlines
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
   tests_Hledger_Read_JournalReader,
   tests_Hledger_Read_TimelogReader,
   tests_Hledger_Read_CsvReader,

   "journal" ~: do
    r <- runExceptT $ parseWithCtx nullctx JournalReader.journal ""
    assertBool "journal should parse an empty file" (isRight $ r)
    jE <- readJournal Nothing Nothing True Nothing "" -- don't know how to get it from journal
    either error' (assertBool "journal parsing an empty file should give an empty journal" . null . jtxns) jE

  ]
