{-|
Utilities for doing I/O with ledger files.
-}

module Ledger.IO
where
import Control.Monad.Error
import Ledger.Ledger (cacheLedger)
import Ledger.Parse (parseLedger)
import Ledger.RawLedger (canonicaliseAmounts,filterRawLedger,rawLedgerSelectingDate)
import Ledger.Types (FilterSpec(..),WhichDate(..),DateSpan(..),RawLedger(..),Ledger(..))
import Ledger.Utils (getCurrentLocalTime)
import System.Directory (getHomeDirectory)
import System.Environment (getEnv)
import System.IO
import System.FilePath ((</>))


ledgerenvvar           = "LEDGER"
timelogenvvar          = "TIMELOG"
ledgerdefaultfilename  = ".ledger"
timelogdefaultfilename = ".timelog"

nullfilterspec = FilterSpec {
                  datespan=DateSpan Nothing Nothing
                 ,cleared=Nothing
                 ,real=False
                 ,costbasis=False
                 ,acctpats=[]
                 ,descpats=[]
                 ,whichdate=ActualDate
                 }

-- | Get the user's default ledger file path.
myLedgerPath :: IO String
myLedgerPath = 
    getEnv ledgerenvvar `catch` 
               (\_ -> do
                  home <- getHomeDirectory
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
readLedger = readLedgerWithFilterSpec nullfilterspec

-- | Read a ledger from this file, filtering according to the filter spec.,
-- | or give an error.
readLedgerWithFilterSpec :: FilterSpec -> FilePath -> IO Ledger
readLedgerWithFilterSpec fspec f = do
  s <- readFile f 
  rl <- rawLedgerFromString s
  return $ filterAndCacheLedger fspec s rl

-- | Read a RawLedger from the given string, using the current time as
-- reference time, or give a parse error.
rawLedgerFromString :: String -> IO RawLedger
rawLedgerFromString s = do
  t <- getCurrentLocalTime
  liftM (either error id) $ runErrorT $ parseLedger t "(string)" s

-- | Convert a RawLedger to a canonicalised, cached and filtered Ledger.
filterAndCacheLedger :: FilterSpec -> String -> RawLedger -> Ledger
filterAndCacheLedger (FilterSpec{datespan=datespan,cleared=cleared,real=real,
                                 costbasis=costbasis,acctpats=acctpats,
                                 descpats=descpats,whichdate=whichdate})
                     rawtext
                     rl = 
    (cacheLedger acctpats 
    $ filterRawLedger datespan descpats cleared real 
    $ rawLedgerSelectingDate whichdate
    $ canonicaliseAmounts costbasis rl
    ){rawledgertext=rawtext}

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

