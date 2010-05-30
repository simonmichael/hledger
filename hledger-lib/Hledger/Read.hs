{-# LANGUAGE CPP #-}
{-| 

Read hledger data from various data formats, and related utilities.

-}

module Hledger.Read (
       tests_Hledger_Read,
       module Hledger.Read.Common,
       Hledger.Read.Journal.someamount,
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
import qualified Hledger.Read.Journal (parseJournal,parseJournalFile,ledgerFile,someamount,tests_Journal)
import qualified Hledger.Read.Timelog (tests_Timelog) --parseJournal

import Control.Monad.Error
import System.Directory (getHomeDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Exit
import System.IO (stderr)
#if __GLASGOW_HASKELL__ <= 610
import System.IO.UTF8 (hPutStrLn)
#else
import System.IO (hPutStrLn)
#endif


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

-- | Read a journal from this file, or throw an error.
readJournalFile :: FilePath -> IO Journal
readJournalFile f =
  (runErrorT . Hledger.Read.Journal.parseJournalFile) f >>= either printerror return
  where printerror e = hPutStrLn stderr e >> exitWith (ExitFailure 1)

-- | Read a Journal from this string, or throw an error.
readJournal :: String -> IO Journal
readJournal s =
  (runErrorT . Hledger.Read.Journal.parseJournal "(from string)") s >>= either error return

-- -- | Expand ~ in a file path (does not handle ~name).
-- tildeExpand :: FilePath -> IO FilePath
-- tildeExpand ('~':[])     = getHomeDirectory
-- tildeExpand ('~':'/':xs) = getHomeDirectory >>= return . (++ ('/':xs))
-- --handle ~name, requires -fvia-C or ghc 6.8:
-- --import System.Posix.User
-- -- tildeExpand ('~':xs)     =  do let (user, path) = span (/= '/') xs
-- --                                pw <- getUserEntryForName user
-- --                                return (homeDirectory pw ++ path)
-- tildeExpand xs           =  return xs

tests_Hledger_Read = TestList
  [

   "ledgerFile" ~: do
    assertBool "ledgerFile should parse an empty file" (isRight $ parseWithCtx emptyCtx Hledger.Read.Journal.ledgerFile "")
    r <- readJournal "" -- don't know how to get it from ledgerFile
    assertBool "ledgerFile parsing an empty file should give an empty ledger" $ null $ jtxns r

  ,Hledger.Read.Journal.tests_Journal
  ,Hledger.Read.Timelog.tests_Timelog
  ]
