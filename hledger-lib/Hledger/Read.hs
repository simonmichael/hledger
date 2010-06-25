{-# LANGUAGE CPP #-}
{-| 

Read hledger data from various data formats, and related utilities.

-}

module Hledger.Read (
       tests_Hledger_Read,
       readJournalFile,
       readJournal,
       myLedgerPath,
       myTimelogPath,
       myJournal,
       myTimelog,
)
where
import Hledger.Data.Types (Journal(..))
import Hledger.Data.Utils
import Hledger.Read.Common
import Hledger.Read.Journal as Journal
import Hledger.Read.Timelog as Timelog

import Control.Monad.Error
import Data.Either (partitionEithers)
import Safe (headDef)
import System.Directory (getHomeDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Exit
import System.IO (stderr)
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (readFile, putStr, putStrLn, print, getContents)
import System.IO.UTF8
#else
import System.IO (hPutStrLn)
#endif


ledgerenvvar           = "LEDGER"
timelogenvvar          = "TIMELOG"
ledgerdefaultfilename  = ".ledger"
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
-- data format, or give an error. Tries to parse each known data format in
-- turn. If none succeed, gives the error message specific to the intended
-- data format, which if not specified is guessed from the file suffix and
-- possibly the data.
journalFromPathAndString :: Maybe String -> FilePath -> String -> IO Journal
journalFromPathAndString format fp s = do
  let readers' = case format of Just f -> case readerForFormat f of Just r -> [r]
                                                                    Nothing -> []
                                Nothing -> readers
  (errors, journals) <- partitionEithers `fmap` mapM try readers'
  case journals of j:_ -> return j
                   _   -> hPutStrLn stderr (errMsg errors) >> exitWith (ExitFailure 1)
    where
      try r = (runErrorT . (rParser r) fp) s
      errMsg [] = unknownFormatMsg
      errMsg es = printf "could not parse %s data in %s\n%s" (rFormat r) fp e
          where (r,e) = headDef (head readers, head es) $ filter detects $ zip readers es
                detects (r,_) = (rDetector r) fp s
      unknownFormatMsg = printf "could not parse %sdata in %s" (fmt formats) fp
          where fmt [] = ""
                fmt [f] = f ++ " "
                fmt fs = intercalate ", " (init fs) ++ " or " ++ last fs ++ " "

-- | Read a journal from this file, using the specified data format or
-- trying all known formats, or give an error.
readJournalFile :: Maybe String -> FilePath -> IO Journal
readJournalFile format "-" = getContents >>= journalFromPathAndString format "(stdin)"
readJournalFile format f   = readFile f  >>= journalFromPathAndString format f

-- | Read a Journal from this string, using the specified data format or
-- trying all known formats, or give an error.
readJournal :: Maybe String -> String -> IO Journal
readJournal format s = journalFromPathAndString format "(string)" s

-- | Get the user's default ledger file path.
myLedgerPath :: IO String
myLedgerPath = 
    getEnv ledgerenvvar `catch` 
               (\_ -> do
                  home <- getHomeDirectory `catch` (\_ -> return "")
                  return $ home </> ledgerdefaultfilename)
  
-- | Get the user's default timelog file path.
myTimelogPath :: IO String
myTimelogPath =
    getEnv timelogenvvar `catch`
               (\_ -> do
                  home <- getHomeDirectory
                  return $ home </> timelogdefaultfilename)

-- | Read the user's default journal file, or give an error.
myJournal :: IO Journal
myJournal = myLedgerPath >>= readJournalFile Nothing

-- | Read the user's default timelog file, or give an error.
myTimelog :: IO Journal
myTimelog = myTimelogPath >>= readJournalFile Nothing

tests_Hledger_Read = TestList
  [

   "ledgerFile" ~: do
    assertBool "ledgerFile should parse an empty file" (isRight $ parseWithCtx emptyCtx Journal.ledgerFile "")
    r <- readJournal Nothing "" -- don't know how to get it from ledgerFile
    assertBool "ledgerFile parsing an empty file should give an empty ledger" $ null $ jtxns r

  ,Journal.tests_Journal
  ,Timelog.tests_Timelog
  ]
