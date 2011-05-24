{-# LANGUAGE CPP #-}
{-|
hledger-web - a hledger add-on providing a web interface.
Copyright (c) 2007-2011 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.
-}

module Main
where

import Controller (withApp)
import Network.Wai.Handler.Warp (run)
#if PRODUCTION
#else
import Network.Wai.Middleware.Debug (debug)
#endif

import Prelude hiding (putStr, putStrLn)
-- import Control.Concurrent (forkIO, threadDelay)
import Data.Text(pack)
import System.Exit (exitFailure)
import System.IO.Storage (withStore, putValue,)
import System.Console.GetOpt
import Yesod.Helpers.Static

import Hledger.Cli.Options
import Hledger.Cli.Utils (withJournalDo) --, openBrowserOn)
import Hledger.Cli.Version (progversionstr, binaryfilename)
import Hledger.Data
import Hledger.Data.UTF8 (putStr, putStrLn)

import App
import EmbeddedFiles (createFilesIfMissing)
import Settings (defhost, defport, datadir, staticdir) -- , browserstartdelay)


progname_web = progname_cli ++ "-web"

options_web :: [OptDescr Opt]
options_web = [
  Option ""  ["base-url"]     (ReqArg BaseUrl "URL") "use this base url (default http://localhost:PORT)"
 ,Option ""  ["port"]         (ReqArg Port "N")      "serve on tcp port N (default 5000)"
 ]

usage_preamble_web =
  "Usage: hledger-web [OPTIONS] [PATTERNS]\n" ++
  "\n" ++
  "Reads your ~/.journal file, or another specified by $LEDGER or -f, and\n" ++
  "starts a web ui server. Also attempts to start a web browser (unless --debug).\n" ++
  "\n"

usage_options_web = usageInfo "hledger-web options:" options_web ++ "\n"

usage_web = concat [
             usage_preamble_web
            ,usage_options_web
            ,usage_options_cli
            ,usage_postscript_cli
            ]

main :: IO ()
main = do
  (opts, args) <- parseArgumentsWith $ options_cli++options_web
  run opts args
    where
      run opts args
       | Help `elem` opts             = putStr usage_web
       | Version `elem` opts          = putStrLn $ progversionstr progname_web
       | BinaryFilename `elem` opts   = putStrLn $ binaryfilename progname_web
       | otherwise                    = withJournalDo opts args "web" web

-- | The web command.
web :: [Opt] -> [String] -> Journal -> IO ()
web opts args j = do
  created <- createFilesIfMissing
  if created
   then do
     putStrLn $ "Installing support files in "++datadir++" - done, please run again."
     exitFailure
   else do
     putStrLn $ "Using support files in "++datadir
     let host    = defhost
         port    = fromMaybe defport $ portFromOpts opts
         baseurl = fromMaybe (printf "http://%s:%d" host port) $ baseUrlFromOpts opts
     -- unless (Debug `elem` opts) $ forkIO (browser baseurl) >> return ()
     server baseurl port opts args j

-- browser :: String -> IO ()
-- browser baseurl = do
--   threadDelay $ fromIntegral browserstartdelay
--   putStrLn "Attempting to start a web browser"
--   openBrowserOn baseurl >> return ()

server :: String -> Int -> [Opt] -> [String] -> Journal -> IO ()
server baseurl port opts args j = do
  printf "Starting http server on port %d with base url %s\n" port baseurl
  let a = App{getStatic=static staticdir
             ,appRoot=pack baseurl
             ,appOpts=opts
             ,appArgs=args
             ,appJournal=j
             }
  withStore "hledger" $ do
    putValue "hledger" "journal" j
#if PRODUCTION
    withApp a (run port)
#else
    withApp a (run port . debug)
#endif
