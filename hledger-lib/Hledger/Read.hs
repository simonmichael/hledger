{-# LANGUAGE CPP #-}
{-| 

Read hledger data from various data formats, and related utilities.

-}

module Hledger.Read (
       tests_Hledger_Read,
       readJournalFile,
       readJournal,
       journalFromPathAndString,
       ledgeraccountname,
       myJournalPath,
       myTimelogPath,
       myJournal,
       myTimelog,
       someamount,
       journalenvvar,
       journaldefaultfilename
)
where
import Control.Monad.Error
import Data.Either (partitionEithers)
import Data.List
import Safe (headDef)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (IOMode(..), withFile, stderr)
import Test.HUnit
import Text.Printf

import Hledger.Data.Dates (getCurrentDay)
import Hledger.Data.Types (Journal(..), Reader(..))
import Hledger.Data.Journal (nullctx)
import Hledger.Read.JournalReader as JournalReader
import Hledger.Read.TimelogReader as TimelogReader
import Hledger.Utils
import Prelude hiding (getContents)
import Hledger.Utils.UTF8 (getContents, hGetContents)


journalenvvar           = "LEDGER_FILE"
journalenvvar2          = "LEDGER"
timelogenvvar           = "TIMELOG"
journaldefaultfilename  = ".hledger.journal"
timelogdefaultfilename = ".hledger.timelog"

-- Here are the available readers. The first is the default, used for unknown data formats.
readers :: [Reader]
readers = [
  JournalReader.reader
 ,TimelogReader.reader
 ]

formats   = map rFormat readers

readerForFormat :: String -> Maybe Reader
readerForFormat s | null rs = Nothing
                  | otherwise = Just $ head rs
    where 
      rs = filter ((s==).rFormat) readers :: [Reader]

-- | Read a Journal from this string (and file path), auto-detecting the
-- data format, or give a useful error string. Tries to parse each known
-- data format in turn. If none succeed, gives the error message specific
-- to the intended data format, which if not specified is guessed from the
-- file suffix and possibly the data.
journalFromPathAndString :: Maybe String -> FilePath -> String -> IO (Either String Journal)
journalFromPathAndString format fp s = do
  let readers' = case format of Just f -> case readerForFormat f of Just r -> [r]
                                                                    Nothing -> []
                                Nothing -> readers
  (errors, journals) <- partitionEithers `fmap` mapM tryReader readers'
  case journals of j:_ -> return $ Right j
                   _   -> return $ Left $ errMsg errors
    where
      tryReader r = (runErrorT . (rParser r) fp) s
      errMsg [] = unknownFormatMsg
      errMsg es = printf "could not parse %s data in %s\n%s" (rFormat r) fp e
          where (r,e) = headDef (head readers, head es) $ filter detects $ zip readers es
                detects (r,_) = (rDetector r) fp s
      unknownFormatMsg = printf "could not parse %sdata in %s" (fmt formats) fp
          where fmt [] = ""
                fmt [f] = f ++ " "
                fmt fs = intercalate ", " (init fs) ++ " or " ++ last fs ++ " "

-- | Read a journal from this file, using the specified data format or
-- trying all known formats, or give an error string; also create the file
-- if it doesn't exist.
readJournalFile :: Maybe String -> FilePath -> IO (Either String Journal)
readJournalFile format "-" = getContents >>= journalFromPathAndString format "(stdin)"
readJournalFile format f = do
  ensureJournalFile f
  withFile f ReadMode $ \h -> hGetContents h >>= journalFromPathAndString format f

-- | Ensure there is a journal at the given file path, creating an empty one if needed.
ensureJournalFile :: FilePath -> IO ()
ensureJournalFile f = do
  exists <- doesFileExist f
  when (not exists) $ do
    hPrintf stderr "No journal file \"%s\", creating it.\n" f
    hPrintf stderr "Edit this file or use \"hledger add\" or \"hledger web\" to add transactions.\n"
    emptyJournal >>= writeFile f

-- | Give the content for a new auto-created journal file.
emptyJournal :: IO String
emptyJournal = do
  d <- getCurrentDay
  return $ printf "; journal created %s by hledger\n\n" (show d)

-- | Read a Journal from this string, using the specified data format or
-- trying all known formats, or give an error string.
readJournal :: Maybe String -> String -> IO (Either String Journal)
readJournal format s = journalFromPathAndString format "(string)" s

-- | Get the user's journal file path. Like ledger, we look first for the
-- LEDGER_FILE environment variable, and if that does not exist, for the
-- legacy LEDGER environment variable. If neither is set, or the value is
-- blank, return the default journal file path, which is
-- ".hledger.journal" in the users's home directory, or if we cannot
-- determine that, in the current directory.
myJournalPath :: IO String
myJournalPath = do
  s <- envJournalPath
  if null s then defaultJournalPath else return s
    where
      envJournalPath = getEnv journalenvvar `catch` (\_ -> getEnv journalenvvar2 `catch` (\_ -> return ""))
      defaultJournalPath = do
                  home <- getHomeDirectory `catch` (\_ -> return "")
                  return $ home </> journaldefaultfilename
  
-- | Get the user's default timelog file path.
myTimelogPath :: IO String
myTimelogPath =
    getEnv timelogenvvar `catch`
               (\_ -> do
                  home <- getHomeDirectory
                  return $ home </> timelogdefaultfilename)

-- | Read the user's default journal file, or give an error.
myJournal :: IO Journal
myJournal = myJournalPath >>= readJournalFile Nothing >>= either error' return

-- | Read the user's default timelog file, or give an error.
myTimelog :: IO Journal
myTimelog = myTimelogPath >>= readJournalFile Nothing >>= either error' return

tests_Hledger_Read = TestList
  [
   tests_Hledger_Read_JournalReader,
   tests_Hledger_Read_TimelogReader,

   "journalFile" ~: do
    assertBool "journalFile should parse an empty file" (isRight $ parseWithCtx nullctx JournalReader.journalFile "")
    jE <- readJournal Nothing "" -- don't know how to get it from journalFile
    either error' (assertBool "journalFile parsing an empty file should give an empty journal" . null . jtxns) jE

  ]
