{-# LANGUAGE CPP #-}
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
  withJournalDoWeb defwebopts (\o j -> defaultDevelApp loader $ makeApplication o j)
  where
    loader =
      Yesod.Default.Config.loadConfig
        (configSettings Development) {csParseExtra = parseExtra}

runWith :: WebOpts -> IO ()
runWith opts
  | "help"            `inRawOpts` rawopts_ (cliopts_ opts) = putStr (showModeUsage webmode) >> exitSuccess
  | "version"         `inRawOpts` rawopts_ (cliopts_ opts) = putStrLn prognameandversion >> exitSuccess
  | "binary-filename" `inRawOpts` rawopts_ (cliopts_ opts) = putStrLn (binaryfilename progname)
  | otherwise = withJournalDoWeb opts web

-- | A version of withJournalDo specialised for hledger-web.
-- Disallows the special - file to avoid some bug,
-- takes WebOpts rather than CliOpts.
withJournalDoWeb :: WebOpts -> (WebOpts -> Journal -> IO a) -> IO a
withJournalDoWeb opts@WebOpts {cliopts_ = copts} cmd = do
  journalpaths <- journalFilePathFromOpts copts

  -- https://github.com/simonmichael/hledger/issues/202
  -- -f- gives [Error#yesod-core] <stdin>: hGetContents: illegal operation (handle is closed)
  -- Also we may try to write to this file. Just disallow -.
  when ("-" `elem` journalpaths) $  -- always non-empty
    error' "hledger-web doesn't support -f -, please specify a file path"
  mapM_ requireJournalFileExists journalpaths

  -- keep synced with withJournalDo  TODO refactor
  readJournalFiles (inputopts_ copts) journalpaths
  >>= mapM (journalTransform copts)
  >>= either error' (cmd opts)

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

