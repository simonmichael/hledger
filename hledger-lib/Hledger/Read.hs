{-# LANGUAGE CPP #-}
{-| 

Read hledger data from various data formats, and related utilities.

-}

module Hledger.Read (
       tests_Hledger_Read,
       readJournalFile,
       readJournal,
       journalFromPathAndString,
       myJournalPath,
       myTimelogPath,
       myJournal,
       myTimelog,
)
where
import Hledger.Data.Dates (getCurrentDay)
import Hledger.Data.Types (Journal(..))
import Hledger.Data.Utils
import Hledger.Read.Common
import Hledger.Read.Journal as Journal
import Hledger.Read.Timelog as Timelog

import Control.Monad.Error
import Data.Either (partitionEithers)
import Safe (headDef)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (IOMode(..), withFile, hGetContents, stderr)
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (readFile, putStr, putStrLn, print, getContents)
import System.IO.UTF8
#else
import System.IO (hPutStrLn)
#endif


journalenvvar           = "LEDGER"
timelogenvvar          = "TIMELOG"
journaldefaultfilename  = ".journal"
timelogdefaultfilename = ".timelog"

-- Here are the available readers. The first is the default, used for unknown data formats.
readers :: [Reader]
readers = [
  Journal.reader
 ,Timelog.reader
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
                   _   -> let s = errMsg errors in hPutStrLn stderr s >> return (Left s)
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

-- | Get the user's default journal file path.
myJournalPath :: IO String
myJournalPath =
    getEnv journalenvvar `catch`
               (\_ -> do
                  home <- getHomeDirectory `catch` (\_ -> return "")
                  return $ home </> journaldefaultfilename)
  
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

   "journalFile" ~: do
    assertBool "journalFile should parse an empty file" (isRight $ parseWithCtx emptyCtx Journal.journalFile "")
    jE <- readJournal Nothing "" -- don't know how to get it from journalFile
    either error' (assertBool "journalFile parsing an empty file should give an empty journal" . null . jtxns) jE

  ,Journal.tests_Journal
  ,Timelog.tests_Timelog
  ]
