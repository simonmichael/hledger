{-# LANGUAGE CPP #-}
{-|
Utilities for doing I/O with ledger files.
-}

module Hledger.Data.IO
where
import Control.Monad.Error
import Hledger.Data.Ledger (makeUncachedLedger)
import Hledger.Data.Parse (parseLedger)
import Hledger.Data.Types (FilterSpec(..),WhichDate(..),Journal(..),Ledger(..))
import Hledger.Data.Utils (getCurrentLocalTime)
import Hledger.Data.Dates (nulldatespan)
import System.Directory (getHomeDirectory)
import System.Environment (getEnv)
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (readFile)
import System.IO.UTF8
#endif
import System.FilePath ((</>))
import System.Time (getClockTime)


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

-- | Read the user's default ledger file, or give an error.
myLedger :: IO Ledger
myLedger = myLedgerPath >>= readLedger

-- | Read the user's default timelog file, or give an error.
myTimelog :: IO Ledger
myTimelog = myTimelogPath >>= readLedger

-- | Read an unfiltered, uncached ledger from this file, or give an error.
readLedger :: FilePath -> IO Ledger
readLedger f = do
  t <- getClockTime
  s <- readFile f
  j <- journalFromString s
  return $ makeUncachedLedger False f t s j

-- -- | Read a ledger from this file, filtering according to the filter spec.,
-- -- | or give an error.
-- readLedgerWithFilterSpec :: FilterSpec -> FilePath -> IO Ledger
-- readLedgerWithFilterSpec fspec f = do
--   s <- readFile f
--   t <- getClockTime
--   j <- journalFromString s
--   return $ filterAndCacheLedger fspec s j{filepath=f, filereadtime=t}

-- | Read a Journal from the given string, using the current time as
-- reference time, or give a parse error.
journalFromString :: String -> IO Journal
journalFromString s = do
  t <- getCurrentLocalTime
  liftM (either error id) $ runErrorT $ parseLedger t "(string)" s

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

