{-# LANGUAGE CPP #-}
{-|
hledger-web - a hledger add-on providing a web interface.
Copyright (c) 2007-2011 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.
-}

module Main
where

-- import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.Maybe
import Data.Text(pack)
import Network.Wai.Handler.Warp (run)
#if PRODUCTION
#else
import Network.Wai.Middleware.Debug (debug)
#endif
import System.Exit
import System.IO.Storage (withStore, putValue)
import Text.Printf
import Yesod.Helpers.Static

import Hledger
import Hledger.Cli hiding (progname,progversion)
import Hledger.Cli.Tests
import Prelude hiding (putStrLn)
import Hledger.Utils.UTF8 (putStrLn)
import Hledger.Web


main :: IO ()
main = do
  opts <- getHledgerWebOpts
  when (debug_ $ cliopts_ opts) $ printf "%s\n" progversion >> printf "opts: %s\n" (show opts)
  runWith opts

runWith :: WebOpts -> IO ()
runWith opts = run opts
    where
      run opts
          | "help" `in_` (rawopts_ $ cliopts_ opts)            = putStr (showModeHelp webmode) >> exitSuccess
          | "version" `in_` (rawopts_ $ cliopts_ opts)         = putStrLn progversion >> exitSuccess
          | "binary-filename" `in_` (rawopts_ $ cliopts_ opts) = putStrLn (binaryfilename progname)
          | otherwise                                          = journalFilePathFromOpts (cliopts_ opts) >>= ensureJournalFile >> withJournalDo' opts web

withJournalDo' :: WebOpts -> (WebOpts -> Journal -> IO ()) -> IO ()
withJournalDo' opts cmd = do
  journalFilePathFromOpts (cliopts_ opts) >>= readJournalFile Nothing >>=
    either error' (cmd opts . journalApplyAliases (aliasesFromOpts $ cliopts_ opts))

-- | The web command.
web :: WebOpts -> Journal -> IO ()
web opts j = do
  created <- createFilesIfMissing
  if created
   then do
     putStrLn $ "Installing support files in "++datadir++" - done, please run again."
     exitFailure
   else do
     putStrLn $ "Running self-tests..."
     runTestsOrExit $ cliopts_ opts
     putStrLn $ "Using support files in "++datadir
     -- unless (debug_ $ cliopts_ opts) $ forkIO (browser baseurl) >> return ()
     server (base_url_ opts) (port_ opts) opts j

-- browser :: String -> IO ()
-- browser baseurl = do
--   threadDelay $ fromIntegral browserstartdelay
--   putStrLn "Attempting to start a web browser"
--   openBrowserOn baseurl >> return ()

server :: String -> Int -> WebOpts -> Journal -> IO ()
server baseurl port opts j = do
  printf "Starting http server on port %d with base url %s\n" port baseurl
  let a = App{getStatic=static staticdir
             ,appRoot=pack baseurl
             ,appOpts=opts
             ,appArgs=patterns_ $ reportopts_ $ cliopts_ opts
             ,appJournal=j
             }
  withStore "hledger" $ do
    putValue "hledger" "journal" j
    return ()
#if PRODUCTION
    withApp a (run port)
#else
    withApp a (run port . debug)
#endif
