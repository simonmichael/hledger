{-|

hledger-web - a hledger add-on providing a web interface.
Copyright (c) 2007-2012 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

-}

module Hledger.Web.Main
where

-- yesod scaffold imports
import Prelude              (IO)
import Yesod.Default.Config --(fromArgs)
-- import Yesod.Default.Main   (defaultMain)
import Settings            --  (parseExtra)
import Application          (makeApplication)
import Data.Conduit.Network (HostPreference(HostIPv4))
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsPort)
--
import Prelude hiding (putStrLn)
import Control.Monad (when)
import Data.Text (pack)
import System.Exit (exitSuccess)
import System.IO.Storage (withStore, putValue)
import Text.Printf

import Hledger
import Hledger.Utils.UTF8IOCompat (putStrLn)
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.Web.Options


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
  | otherwise = do
    requireJournalFileExists =<< journalFilePathFromOpts (cliopts_ opts)
    withJournalDo' opts web

withJournalDo' :: WebOpts -> (WebOpts -> Journal -> IO ()) -> IO ()
withJournalDo' opts cmd = do
  journalFilePathFromOpts (cliopts_ opts) >>= readJournalFile Nothing Nothing >>=
    either error' (cmd opts . journalApplyAliases (aliasesFromOpts $ cliopts_ opts))

-- | The web command.
web :: WebOpts -> Journal -> IO ()
web opts j = do
  -- unless (debug_ $ cliopts_ opts) $ forkIO (browser baseurl) >> return ()
  d <- getCurrentDay
  let j' = filterJournalTransactions (queryFromOpts d $ reportopts_ $ cliopts_ opts) j
  server (base_url_ opts) (port_ opts) opts j'

-- browser :: String -> IO ()
-- browser baseurl = do
--   threadDelay $ fromIntegral browserstartdelay
--   putStrLn "Attempting to start a web browser"
--   openBrowserOn baseurl >> return ()

server :: String -> Int -> WebOpts -> Journal -> IO ()
server baseurl port opts j = do
  _ <- printf "Starting http server on port %d with base url %s\n" port baseurl
  -- let a = App{getStatic=static staticdir
  --            ,appRoot=pack baseurl
  --            ,appOpts=opts
  --            ,appArgs=patterns_ $ reportopts_ $ cliopts_ opts
  --            ,appJournal=j
  --            }
  withStore "hledger" $ do
    putValue "hledger" "journal" j

-- defaultMain (fromArgs parseExtra) makeApplication
    app <- makeApplication (AppConfig {
              appEnv = Development
            , appPort = port_ opts
            , appRoot = pack baseurl
            , appHost = HostIPv4
            , appExtra = Extra "" Nothing
            })
    runSettings defaultSettings
        { settingsPort = port_ opts
        } app
