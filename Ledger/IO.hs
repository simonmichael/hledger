{-|
Utilities for doing I/O with ledger files.
-}

module Ledger.IO
where
import Control.Monad.Error
import Data.Time.Clock
import Data.Time.LocalTime (LocalTime)
import Ledger.Ledger (cacheLedger)
import Ledger.Parse (parseLedger)
import Ledger.RawLedger (canonicaliseAmounts,filterRawLedger)
import Ledger.Types (DateSpan(..),RawLedger,Ledger(..))
import Ledger.Utils (getCurrentLocalTime)
import System.Directory (getHomeDirectory)
import System.Environment (getEnv)
import System.IO
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map (lookup)
import System.FilePath ((</>))


ledgerenvvar           = "LEDGER"
timelogenvvar          = "TIMELOG"
ledgerdefaultfilename  = ".ledger"
timelogdefaultfilename = ".timelog"

-- | A tuple of arguments specifying how to filter a raw ledger file:
-- 
-- - only include transactions in this date span
-- 
-- - only include if cleared\/uncleared\/don't care
-- 
-- - only include if real\/don't care
-- 
-- - convert all amounts to cost basis
-- 
-- - only include if matching these account patterns
-- 
-- - only include if matching these description patterns

type IOArgs = (DateSpan
              ,Maybe Bool
              ,Bool
              ,Bool
              ,[String]
              ,[String]
              )

noioargs = (DateSpan Nothing Nothing, Nothing, False, False, [], [])

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
readLedger = readLedgerWithIOArgs noioargs

-- | Read a ledger from this file, filtering according to the io args,
-- | or give an error.
readLedgerWithIOArgs :: IOArgs -> FilePath -> IO Ledger
readLedgerWithIOArgs ioargs f = do
  t <- getCurrentLocalTime
  s <- readFile f 
  rl <- rawLedgerFromString s
  return $ filterAndCacheLedger ioargs s rl

-- | Read a RawLedger from the given string, using the current time as
-- reference time, or give a parse error.
rawLedgerFromString :: String -> IO RawLedger
rawLedgerFromString s = do
  t <- getCurrentLocalTime
  liftM (either error id) $ runErrorT $ parseLedger t "(string)" s

-- | Convert a RawLedger to a canonicalised, cached and filtered Ledger.
filterAndCacheLedger :: IOArgs -> String -> RawLedger -> Ledger
filterAndCacheLedger (span,cleared,real,costbasis,apats,dpats) rawtext rl = 
    (cacheLedger apats 
    $ filterRawLedger span dpats cleared real 
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

