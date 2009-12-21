{-|
Utilities for doing I/O with ledger files.
-}

module Ledger.IO
where
import Control.Monad.Error
import Ledger.Ledger (cacheLedger', nullledger)
import Ledger.Parse (parseLedger)
import Ledger.Types (FilterSpec(..),WhichDate(..),Journal(..),Ledger(..))
import Ledger.Utils (getCurrentLocalTime)
import Ledger.Dates (nulldatespan)
import System.Directory (getHomeDirectory)
import System.Environment (getEnv)
import System.IO
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

-- | Read a ledger from this file, with no filtering, or give an error.
readLedger :: FilePath -> IO Ledger
readLedger f = do
  t <- getClockTime
  s <- readFile f
  j <- journalFromString s
  return $ cacheLedger' $ nullledger{journal=j{filepath=f,filereadtime=t,jtext=s}}

-- -- | Read a ledger from this file, filtering according to the filter spec.,
-- -- | or give an error.
-- readLedgerWithFilterSpec :: FilterSpec -> FilePath -> IO Ledger
-- readLedgerWithFilterSpec fspec f = do
--   s <- readFile f
--   t <- getClockTime
--   rl <- journalFromString s
--   return $ filterAndCacheLedger fspec s rl{filepath=f, filereadtime=t}

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

