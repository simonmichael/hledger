{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp
import           Safe
import           Servant
import           System.Console.Docopt
import           System.Environment (getArgs)
import           System.IO
import           Text.Printf

import Hledger.Cli hiding (Reader, version)
import System.Exit

version="0.27.98"

doc :: Docopt
doc = [docopt|
hledger-api 0.27.98

Usage:
  hledger-api [options]
  hledger-api --version
  hledger-api --help

Options:
  -f --file FILE  use a different input file
  -p --port PORT  use a different TCP port (default: 8001)
     --version    show version
  -h --help       show this help
|]

main :: IO ()
main = do
  args <- getArgs >>= parseArgsOrExit doc
  when (isPresent args (longOption "help")) $ exitWithUsage doc
  when (isPresent args (longOption "version")) $ putStrLn version >> exitSuccess
  let defp = "8001"
  p <- case readMay $ getArgWithDefault args defp (longOption "port") of
        Nothing -> exitWithUsage doc
        Just n  -> return n
  deff <- defaultJournalPath
  let f = getArgWithDefault args deff (longOption "file")
  requireJournalFileExists f
  readJournalFile Nothing Nothing True f >>= either error' (serveApi f p)

-- serveApi :: CliOpts -> Journal -> IO ()
serveApi :: FilePath -> Int -> Journal -> IO ()
serveApi f p j = do
  -- d <- getCurrentDay
  -- let j' =
  --       filterJournalTransactions (queryFromOpts d $ reportopts_ opts) $
  --       journalApplyAliases (aliasesFromOpts opts) $
  --       j
  printf "Starting web api serving %s on port %d\nPress ctrl-c to quit\n" f p >> hFlush stdout
  Warp.run p $ hledgerApiApp j

hledgerApiApp :: Journal -> Wai.Application
hledgerApiApp j = Servant.serve hledgerApi hledgerApiServer
  where
    hledgerApi :: Proxy HledgerApi
    hledgerApi = Proxy

    hledgerApiServer :: Servant.Server HledgerApi
    hledgerApiServer = Servant.enter readerToEither hledgerServerT
      where
        readerToEither :: Reader Journal :~> EitherT ServantErr IO
        readerToEither = Nat $ \r -> return (runReader r j)

type HledgerApi =
  "accounts" :> Get '[JSON] [AccountName]

hledgerServerT :: ServerT HledgerApi (Reader Journal)
hledgerServerT =
  accountsH
  where
    accountsH = do
      j <- ask
      return $ journalAccountNames j

