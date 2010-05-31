{-# LANGUAGE CPP #-}
{-| 

Read hledger data from various data formats, and related utilities.

-}

module Hledger.Read (
       tests_Hledger_Read,
       myLedgerPath,
       myTimelogPath,
       myJournal,
       myTimelog,
       readJournalFile,
       readJournal,
)
where
import Hledger.Data.Types (Journal(..))
import Hledger.Data.Utils
import Hledger.Read.Common
import qualified Hledger.Read.Journal (parseJournal,ledgerFile,tests_Journal)
import qualified Hledger.Read.Timelog (parseJournal,tests_Timelog)

import Control.Monad.Error
import Data.Either (partitionEithers)
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


formats = [
  "journal"
 ,"timelog"
-- ,"csv"
 ]

unknownformatmsg fp = printf "could not recognise %sdata in %s" (fmt formats) fp
    where fmt [] = ""
          fmt [f] = f ++ " "
          fmt fs = intercalate ", " (init fs) ++ " or " ++ last fs ++ " "

parsers = [Hledger.Read.Journal.parseJournal
          ,Hledger.Read.Timelog.parseJournal
          ]

ledgerenvvar           = "LEDGER"
timelogenvvar          = "TIMELOG"
ledgerdefaultfilename  = ".ledger"
timelogdefaultfilename = ".timelog"

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
myJournal = myLedgerPath >>= readJournalFile

-- | Read the user's default timelog file, or give an error.
myTimelog :: IO Journal
myTimelog = myTimelogPath >>= readJournalFile

-- | Read a journal from this file, trying all known data formats,
-- or give an error.
readJournalFile :: FilePath -> IO Journal
readJournalFile "-" = getContents >>= journalFromPathAndString "(stdin)"
readJournalFile f   = readFile f  >>= journalFromPathAndString f

-- | Read a Journal from this string, trying all known data formats, or
-- give an error.
readJournal :: String -> IO Journal
readJournal = journalFromPathAndString "(string)"

-- | Read a Journal from this string, trying each known data format in
-- turn, or give an error.  The file path is also required.
journalFromPathAndString :: FilePath -> String -> IO Journal
journalFromPathAndString f s = do
  (errors, journals) <- partitionEithers `fmap` mapM try parsers
  case journals of j:_ -> return j
                   _   -> hPutStrLn stderr (errmsg errors) >> exitWith (ExitFailure 1)
    where
      try p = (runErrorT . p f) s
      errmsg [] = unknownformatmsg f
      errmsg (e:_) = unlines [unknownformatmsg f, e]

tests_Hledger_Read = TestList
  [

   "ledgerFile" ~: do
    assertBool "ledgerFile should parse an empty file" (isRight $ parseWithCtx emptyCtx Hledger.Read.Journal.ledgerFile "")
    r <- readJournal "" -- don't know how to get it from ledgerFile
    assertBool "ledgerFile parsing an empty file should give an empty ledger" $ null $ jtxns r

  ,Hledger.Read.Journal.tests_Journal
  ,Hledger.Read.Timelog.tests_Timelog
  ]
