{-|

hledger-web - a hledger add-on providing a web interface.
Copyright (c) 2007-2020 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hledger.Web.Main where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.String (fromString)
import qualified Data.Text as T
import Network.Socket
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, runSettingsSocket, defaultSettings, setHost, setPort)
import Network.Wai.Handler.Launch (runHostPortFullUrl)
import Prelude hiding (putStrLn)
import System.Directory (removeFile)
import System.Environment ( getArgs, withArgs )
import System.Exit (exitSuccess, exitFailure)
import System.IO (hFlush, stdout)
import System.PosixCompat.Files (getFileStatus, isSocket)
import Text.Printf (printf)
import Yesod.Default.Config
import Yesod.Default.Main (defaultDevelApp)

import Hledger
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.Utils.UTF8IOCompat (putStrLn)
import Hledger.Web.Application (makeApplication)
import Hledger.Web.Settings (Extra(..), parseExtra)
import Hledger.Web.Test (hledgerWebTest)
import Hledger.Web.WebOptions

-- Run in fast reloading mode for yesod devel.
hledgerWebDev :: IO (Int, Application)
hledgerWebDev =
  withJournalDo (cliopts_ defwebopts) (defaultDevelApp loader . makeApplication defwebopts)
  where
    loader =
      Yesod.Default.Config.loadConfig
        (configSettings Development) {csParseExtra = parseExtra}

-- Run normally.
hledgerWebMain :: IO ()
hledgerWebMain = do
  wopts@WebOpts{cliopts_=copts@CliOpts{debug_, rawopts_}} <- getHledgerWebOpts
  when (debug_ > 0) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show wopts)
  if
    | "help"            `inRawOpts` rawopts_ -> putStr (showModeUsage webmode) >> exitSuccess
    | "info"            `inRawOpts` rawopts_ -> runInfoForTopic "hledger-web" Nothing
    | "man"             `inRawOpts` rawopts_ -> runManForTopic  "hledger-web" Nothing
    | "version"         `inRawOpts` rawopts_ -> putStrLn prognameandversion >> exitSuccess
    | "binary-filename" `inRawOpts` rawopts_ -> putStrLn (binaryfilename progname)
    | "test"            `inRawOpts` rawopts_ -> do
      -- remove --test and --, leaving other args for hspec
      filter (not . (`elem` ["--test","--"])) <$> getArgs >>= flip withArgs hledgerWebTest
    | otherwise                              -> withJournalDo copts (web wopts)

-- | The hledger web command.
web :: WebOpts -> Journal -> IO ()
web opts j = do
  let initq = rsQuery . reportspec_ $ cliopts_ opts
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
  _ <- printf "Serving web %s on %s:%d with base url %s\n"
         (if serve_api_ opts then "API" else "UI and API" :: String) h p u
  if serve_ opts || serve_api_ opts
    then do
      putStrLn "Press ctrl-c to quit"
      hFlush stdout
      let warpsettings = setHost (fromString h) (setPort p defaultSettings)
      case socket_ opts of
        Just s -> do
          if isUnixDomainSocketAvailable then
            bracket
              (do
                  sock <- socket AF_UNIX Stream 0
                  setSocketOption sock ReuseAddr 1
                  bind sock $ SockAddrUnix s
                  listen sock maxListenQueue
                  return sock
              )
              (\_ -> do
                  sockstat <-  getFileStatus s
                  when (isSocket sockstat) $ removeFile s
              )
              (\sock -> Network.Wai.Handler.Warp.runSettingsSocket warpsettings sock app)
            else do
              putStrLn "Unix domain sockets are not available on your operating system"
              putStrLn "Please try again without --socket"
              exitFailure
        Nothing -> Network.Wai.Handler.Warp.runSettings warpsettings app
    else do
      putStrLn "This server will exit after 2m with no browser windows open (or press ctrl-c)"
      putStrLn "Opening web browser..."
      hFlush stdout
      -- exits after 2m of inactivity (hardcoded)
      Network.Wai.Handler.Launch.runHostPortFullUrl h p u app

