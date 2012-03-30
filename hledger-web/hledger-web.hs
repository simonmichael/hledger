{-# LANGUAGE CPP, OverloadedStrings #-}
{-|

hledger-web - a hledger add-on providing a web interface.
Copyright (c) 2007-2012 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

-}

module Main
where

import Data.Conduit.Network (HostPreference(..))
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsPort)
import Yesod.Default.Config
-- import Yesod.Default.Main   (defaultMain)
import Yesod.Logger ({- Logger,-} defaultDevelopmentLogger) --, logString)

import Prelude hiding (putStrLn)
-- -- import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
-- import Data.Maybe
import Data.Text(pack)
import System.Exit
import System.IO.Storage (withStore, putValue)
import Text.Printf

import Hledger
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.Utils.UTF8IOCompat (putStrLn)
import Hledger.Web hiding (opts,j)


main :: IO ()
main = do
  opts <- getHledgerWebOpts
  when (debug_ $ cliopts_ opts) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show opts)
  runWith opts

runWith :: WebOpts -> IO ()
runWith opts
  | "help" `in_` (rawopts_ $ cliopts_ opts)            = putStr (showModeHelp webmode) >> exitSuccess
  | "version" `in_` (rawopts_ $ cliopts_ opts)         = putStrLn prognameandversion >> exitSuccess
  | "binary-filename" `in_` (rawopts_ $ cliopts_ opts) = putStrLn (binaryfilename progname)
  | otherwise                                          = journalFilePathFromOpts (cliopts_ opts) >>= ensureJournalFileExists >> withJournalDo' opts web

withJournalDo' :: WebOpts -> (WebOpts -> Journal -> IO ()) -> IO ()
withJournalDo' opts cmd = do
  journalFilePathFromOpts (cliopts_ opts) >>= readJournalFile Nothing Nothing >>=
    either error' (cmd opts . journalApplyAliases (aliasesFromOpts $ cliopts_ opts))

-- | The web command.
web :: WebOpts -> Journal -> IO ()
web opts j = do
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
  -- let a = App{getStatic=static staticdir
  --            ,appRoot=pack baseurl
  --            ,appOpts=opts
  --            ,appArgs=patterns_ $ reportopts_ $ cliopts_ opts
  --            ,appJournal=j
  --            }
  withStore "hledger" $ do
    putValue "hledger" "journal" j

-- defaultMain :: (Show env, Read env)
--             => IO (AppConfig env extra)
--             -> (AppConfig env extra -> Logger -> IO Application)
--             -> IO ()
-- defaultMain load getApp = do
    -- config <- fromArgs parseExtra
    let config = AppConfig {
              appEnv = Development
            , appPort = port_ opts
            , appRoot = pack baseurl
            , appHost = HostIPv4
            , appExtra = Extra "" Nothing
            }
    logger <- defaultDevelopmentLogger
    app <- getApplication config logger
    runSettings defaultSettings
        { settingsPort = appPort config
        } app
