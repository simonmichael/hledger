{-# LANGUAGE CPP #-}
{-|
Utilities for doing I/O with ledger files.
-}

module Hledger.Data.IO
where
import Control.Monad.Error
import Hledger.Data.Parse (parseJournal)
import Hledger.Data.Types (FilterSpec(..),WhichDate(..),Journal(..))
import Hledger.Data.Dates (nulldatespan)
import System.Directory (getHomeDirectory)
import System.Environment (getEnv)
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (readFile)
import System.IO.UTF8
#endif
import System.FilePath ((</>))


ledgerenvvar           = "LEDGER"
timelogenvvar          = "TIMELOG"
ledgerdefaultfilename  = ".ledger"
timelogdefaultfilename = ".timelog"

nullfilterspec = FilterSpec {
     datespan=nulldatespan
    ,cleared=Nothing
    ,real=False
    ,empty=False
    ,costbasis=False
    ,acctpats=[]
    ,descpats=[]
    ,whichdate=ActualDate
    ,depth=Nothing
    }

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
myJournal = myLedgerPath >>= readJournal

-- | Read the user's default timelog file, or give an error.
myTimelog :: IO Journal
myTimelog = myTimelogPath >>= readJournal

-- | Read a journal from this file, or give an error.
readJournal :: FilePath -> IO Journal
readJournal f = do
  s <- readFile f
  journalFromString s

-- | Read a Journal from the given string, using the current time as
-- reference time, or throw an error.
journalFromString :: String -> IO Journal
journalFromString s = liftM (either error id) $ runErrorT $ parseJournal "(string)" s

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

