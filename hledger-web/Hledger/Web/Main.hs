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
  when (debug_ (cliopts_ opts) > 0) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show opts)
  runWith opts

hledgerWebDev :: IO (Int, Application)
hledgerWebDev =
  withJournalDo (cliopts_ defwebopts) (defaultDevelApp loader . makeApplication defwebopts)
  where
    loader =
      Yesod.Default.Config.loadConfig
        (configSettings Development) {csParseExtra = parseExtra}

runWith :: WebOpts -> IO ()
runWith opts
  | "help"            `inRawOpts` rawopts_ (cliopts_ opts) = putStr (showModeUsage webmode) >> exitSuccess
  | "version"         `inRawOpts` rawopts_ (cliopts_ opts) = putStrLn prognameandversion >> exitSuccess
  | "binary-filename" `inRawOpts` rawopts_ (cliopts_ opts) = putStrLn (binaryfilename progname)
  | otherwise = withJournalDo (cliopts_ opts) (web opts)

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
  _ <- printf "Serving web %s on %s:%d with base url %s\n"
         (if serve_api_ opts then "API" else "UI and API" :: String) h p u
  if serve_ opts || serve_api_ opts
    then do
      putStrLn "Press ctrl-c to quit"
      hFlush stdout
      let warpsettings = setHost (fromString h) (setPort p defaultSettings)
      Network.Wai.Handler.Warp.runSettings warpsettings app
    else do
      putStrLn "This server will exit after 2m with no browser windows open (or press ctrl-c)"
      putStrLn "Opening web browser..."
      hFlush stdout
      -- exits after 2m of inactivity (hardcoded)
      Network.Wai.Handler.Launch.runHostPortUrl h p "" app

