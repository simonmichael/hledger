{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-|

hledger-web - a hledger add-on providing a web interface.
Copyright (c) 2007-2012 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

-}

module Hledger.Web.Main where

import Control.Monad (when)
import Data.String (fromString)
import qualified Data.Text as T
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setHost, setPort)
import Network.Wai.Handler.Launch (runHostPortUrl)
import Prelude hiding (putStrLn)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Yesod.Default.Config
import Yesod.Default.Main (defaultDevelApp)

import Hledger
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.Utils.UTF8IOCompat (putStrLn)
import Hledger.Web.Application (makeApplication)
import Hledger.Web.Settings (Extra(..), parseExtra)
import Hledger.Web.WebOptions


hledgerWebMain :: IO ()
hledgerWebMain = do
  opts <- getHledgerWebOpts
  let copts = cliopts_ opts
  when (debug_ copts > 0) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show opts)
  if
    | "help"            `inRawOpts` rawopts_ copts -> putStr (showModeUsage webmode) >> exitSuccess
    | "version"         `inRawOpts` rawopts_ copts -> putStrLn prognameandversion >> exitSuccess
    | "binary-filename" `inRawOpts` rawopts_ copts -> putStrLn (binaryfilename progname)
    | otherwise -> do
        mapM_ ensureJournalFileExists =<< journalFilePathFromOpts copts
        withJournalDo copts (web opts)

hledgerWebDev :: IO (Int, Application)
hledgerWebDev =
  withJournalDo (cliopts_ defwebopts) (defaultDevelApp loader . makeApplication defwebopts)
  where
    loader =
      Yesod.Default.Config.loadConfig
        (configSettings Development) {csParseExtra = parseExtra}

-- | The web command.
web :: WebOpts -> Journal -> IO ()
web opts j = do
  d <- getCurrentDay
  let initq = queryFromOpts d $ reportopts_ $ cliopts_ opts
      j' = filterJournalTransactions initq j
      h = host_ opts
      p = port_ opts
      u = base_url_ opts
      staticRoot = T.pack <$> file_url_ opts
      appconfig = AppConfig{appEnv = Development
                           ,appHost = fromString h
                           ,appPort = p
                           ,appRoot = T.pack u
                           ,appExtra = Extra "" Nothing staticRoot
                           }
  app <- makeApplication opts j' appconfig
  -- XXX would like to allow a host name not just an IP address here
  _ <- printf "Starting web app on IP address %s port %d with base url %s\n" h p u
  if serve_ opts
    then do
      putStrLn "Press ctrl-c to quit"
      hFlush stdout
      let warpsettings = setHost (fromString h) (setPort p defaultSettings)
      Network.Wai.Handler.Warp.runSettings warpsettings app
    else do
      putStrLn "Starting web browser..."
      putStrLn "Web app will auto-exit after a few minutes with no browsers (or press ctrl-c)"
      hFlush stdout
      Network.Wai.Handler.Launch.runHostPortUrl h p "" app

