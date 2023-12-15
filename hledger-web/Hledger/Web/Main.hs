{-|
hledger-web - a basic but robust web UI and JSON API server for hledger.

Copyright (c) 2007-2023 Simon Michael <simon@joyful.com> and contributors.
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
  -- try to encourage user's $PAGER to properly display ANSI (in command line help)
  when useColorOnStdout setupPager

  wopts@WebOpts{cliopts_=copts@CliOpts{debug_, rawopts_}} <- getHledgerWebOpts
  when (debug_ > 0) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show wopts)
  if
    | boolopt "help"            rawopts_ -> pager (showModeUsage webmode) >> exitSuccess
    | boolopt "info"            rawopts_ -> runInfoForTopic "hledger-web" Nothing
    | boolopt "man"             rawopts_ -> runManForTopic  "hledger-web" Nothing
    | boolopt "version"         rawopts_ -> putStrLn prognameandversion >> exitSuccess
    -- boolopt "binary-filename" rawopts_ -> putStrLn (binaryfilename progname)
    | boolopt "test"            rawopts_ -> do
      -- remove --test and --, leaving other args for hspec
      (`withArgs` hledgerWebTest) . filter (`notElem` ["--test","--"]) =<< getArgs
    | otherwise                              -> withJournalDo copts (web wopts)

-- | The hledger web command.
web :: WebOpts -> Journal -> IO ()
web opts j = do
  let depthlessinitialq = filterQuery (not . queryIsDepth) . _rsQuery . reportspec_ $ cliopts_ opts
      j' = filterJournalTransactions depthlessinitialq j
      h = host_ opts
      p = port_ opts
      u = base_url_ opts
      staticRoot = T.pack <$> file_url_ opts  -- XXX not used #2139
      appconfig = AppConfig{appEnv = Development
                           ,appHost = fromString h
                           ,appPort = p
                           ,appRoot = T.pack u
                           ,appExtra = Extra "" Nothing staticRoot
                           }
  app <- makeApplication opts j' appconfig

  -- show configuration
  let
    services | serve_api_ opts = "json API"
             | otherwise       = "web UI and json API"
    prettyip ip
        | ip == "127.0.0.1" = ip ++ " (local access)"
        | ip == "0.0.0.0"   = ip ++ " (all interfaces)"
        | otherwise         = ip
    listenat =
      case socket_ opts of
        Just s  -> printf "socket %s" s
        Nothing -> printf "IP address %s, port %d" (prettyip h) p
  printf "Serving %s at %s\nwith base url %s\n" (services::String) (listenat::String) u
  case file_url_ opts of
    Just fu -> printf "and static files base url %s\n" fu
    Nothing -> pure ()

  -- start server and maybe browser
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

