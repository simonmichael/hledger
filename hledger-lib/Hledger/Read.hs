{-| 

This is the entry point to hledger's reading system, which can read
Journals from various data formats. Use this module if you want to
parse journal data or read journal files; it should not be necessary
to import modules below this one.

-}

module Hledger.Read (
       -- * Journal reading utilities
       defaultJournalPath,
       defaultJournal,
       readJournal,
       readJournalFile,
       requireJournalFileExists,
       ensureJournalFileExists,
       -- * Temporary parser exports for Convert
       ledgeraccountname,
       someamount,
       -- * Tests
       tests_Hledger_Read,
)
where
import Control.Monad.Error
import Data.List
import Data.Maybe
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode(..), withFile, stderr)
import Test.HUnit
import Text.Printf

import Hledger.Data.Dates (getCurrentDay)
import Hledger.Data.Types (Journal(..), Reader(..), Format)
import Hledger.Data.Journal (nullctx)
import Hledger.Read.JournalReader as JournalReader
import Hledger.Read.TimelogReader as TimelogReader
import Hledger.Read.CsvReader as CsvReader
import Hledger.Utils
import Prelude hiding (getContents, writeFile)
import Hledger.Utils.UTF8 (getContents, hGetContents, writeFile)


journalEnvVar           = "LEDGER_FILE"
journalEnvVar2          = "LEDGER"
journalDefaultFilename  = ".hledger.journal"

-- The available data file readers, each one handling a particular data
-- format. The first is also used as the default for unknown formats.
readers :: [Reader]
readers = [
  JournalReader.reader
 ,TimelogReader.reader
 -- ,CsvReader.reader
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
      envJournalPath = getEnv journalEnvVar `catch` (\_ -> getEnv journalEnvVar2 `catch` (\_ -> return ""))
      defaultJournalPath = do
                  home <- getHomeDirectory `catch` (\_ -> return "")
                  return $ home </> journalDefaultFilename

-- | Read the default journal file specified by the environment, or raise an error.
defaultJournal :: IO Journal
defaultJournal = defaultJournalPath >>= readJournalFile Nothing Nothing >>= either error' return

-- | Find the reader which can handle the given format, if any.
-- Typically there is just one; only the first is returned.
readerForFormat :: Format -> Maybe Reader
readerForFormat s | null rs = Nothing
                  | otherwise = Just $ head rs
    where 
      rs = filter ((s==).rFormat) readers :: [Reader]

-- | Read a Journal from this string or give an error message, using
-- the specified data format or trying all known formats. CSV
-- conversion rules may be provided for better conversion of that
-- format, and/or a file path for better error messages.
readJournal :: Maybe Format -> Maybe CsvReader.CsvRules -> Maybe FilePath -> String -> IO (Either String Journal)
readJournal format _ path s =
  let readerstotry = case format of Nothing -> readers
                                    Just f -> case readerForFormat f of Just r -> [r]
                                                                        Nothing -> []
  in firstSuccessOrBestError $ map tryReader readerstotry
  where
    path' = fromMaybe "(string)" path
    tryReader :: Reader -> IO (Either String Journal)
    tryReader r = do -- printf "trying %s reader\n" (rFormat r)
                     (runErrorT . (rParser r) path') s

    -- if no reader succeeds, we return the error of the first;
    -- ideally it would be the error of the most likely intended
    -- reader, based on file suffix and/or data sniffing.
    firstSuccessOrBestError :: [IO (Either String Journal)] -> IO (Either String Journal)
    firstSuccessOrBestError []       = return $ Left "no readers found"
    firstSuccessOrBestError attempts = firstSuccessOrBestError' attempts
      where
        firstSuccessOrBestError' [] = head attempts
        firstSuccessOrBestError' (a:as) = do
          r <- a
          case r of Right j -> return $ Right j
                    Left _  -> firstSuccessOrBestError' as

    -- -- unknown format
    -- bestErrorMsg :: [String] -> String -> Maybe FilePath -> String
    -- bestErrorMsg [] _ path = printf "could not parse %sdata%s" fmts pathmsg
    --     where fmts = case formats of
    --                    [] -> ""
    --                    [f] -> f ++ " "
    --                    fs -> intercalate ", " (init fs) ++ " or " ++ last fs ++ " "
    --           pathmsg = case path of
    --                       Nothing -> ""
    --                       Just p -> " in "++p
    -- -- one or more errors - report (the most appropriate ?) one
    -- bestErrorMsg es s path = printf "could not parse %s data%s\n%s" (rFormat r) pathmsg e
    --     where (r,e) = headDef (head readers, head es) $ filter detects $ zip readers es
    --           detects (r,_) = (rDetector r) path' s
    --           pathmsg = case path of
    --                       Nothing -> ""
    --                       Just p -> " in "++p

-- | Read a Journal from this file (or stdin if the filename is -) or
-- give an error message, using the specified data format or trying
-- all known formats. CSV conversion rules may be provided for better
-- conversion of that format.
readJournalFile :: Maybe Format -> Maybe CsvReader.CsvRules -> FilePath -> IO (Either String Journal)
readJournalFile format rules "-" = getContents >>= readJournal format rules (Just "(stdin)")
readJournalFile format rules f = do
  requireJournalFileExists f
  withFile f ReadMode $ \h -> hGetContents h >>= readJournal format rules (Just f)

-- | If the specified journal file does not exist, give a helpful error and quit.
requireJournalFileExists :: FilePath -> IO ()
requireJournalFileExists f = do
  exists <- doesFileExist f
  when (not exists) $ do
    hPrintf stderr "The hledger journal file \"%s\" was not found.\n" f
    hPrintf stderr "Please create it first, eg with hledger add, hledger web, or a text editor.\n"
    hPrintf stderr "Or, specify an existing journal file with -f or LEDGER_FILE.\n"
    exitFailure

-- | Ensure there is a journal file at the given path, creating an empty one if needed.
ensureJournalFileExists :: FilePath -> IO ()
ensureJournalFileExists f = do
  exists <- doesFileExist f
  when (not exists) $ do
    hPrintf stderr "Creating hledger journal file \"%s\".\n" f
    -- note Hledger.Utils.UTF8.* do no line ending conversion on windows,
    -- we currently require unix line endings on all platforms.
    newJournalContent >>= writeFile f

-- | Give the content for a new auto-created journal file.
newJournalContent :: IO String
newJournalContent = do
  d <- getCurrentDay
  return $ printf "; journal created %s by hledger\n" (show d)

tests_Hledger_Read = TestList
  [
   tests_Hledger_Read_JournalReader,
   tests_Hledger_Read_TimelogReader,
   tests_Hledger_Read_CsvReader,

   "journalFile" ~: do
    assertBool "journalFile should parse an empty file" (isRight $ parseWithCtx nullctx JournalReader.journalFile "")
    jE <- readJournal Nothing Nothing Nothing "" -- don't know how to get it from journalFile
    either error' (assertBool "journalFile parsing an empty file should give an empty journal" . null . jtxns) jE

  ]
