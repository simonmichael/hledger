{-# LANGUAGE CPP #-}
{-|
hledger-web - a hledger add-on providing a web interface.
Copyright (c) 2007-2010 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.
-}

module Main where

#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (putStr, putStrLn)
import System.IO.UTF8 (putStr, putStrLn)
#endif
import Control.Concurrent (forkIO, threadDelay)
import Network.Wai.Handler.SimpleServer (run)
import System.Exit (exitFailure) -- , exitWith, ExitCode(ExitSuccess)) -- base 3 compatible
import System.IO.Storage (withStore, putValue,)
import Yesod.Content (typeByExt)
import Yesod.Helpers.Static (fileLookupDir)

import Hledger.Cli.Options
import Hledger.Cli.Utils (withJournalDo, openBrowserOn)
import Hledger.Cli.Version (versionmsg) --, binaryfilename)
import Hledger.Data
import Hledger.Web.App (App(..), withApp)
import Hledger.Web.Files (createFilesIfMissing)
import Hledger.Web.Settings (browserstartdelay, defhost, defport, datadir)


main :: IO ()
main = do
  (opts, cmd, args) <- parseArguments
  run cmd opts args
    where
      run cmd opts args
       | Help `elem` opts             = putStr help1
       | HelpOptions `elem` opts      = putStr help2
       | HelpAll `elem` opts          = putStr $ help1 ++ "\n" ++ help2
       | Version `elem` opts          = putStrLn versionmsg
       -- | BinaryFilename `elem` opts   = putStrLn binaryfilename
       | null cmd                     = maybe (putStr help1) (withJournalDo opts args cmd) defaultcmd
       | cmd `isPrefixOf` "web"       = withJournalDo opts args cmd web
       -- | cmd `isPrefixOf` "test"      = runtests opts args >> return ()
       | otherwise                    = putStr help1

      defaultcmd = Just web

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
     unless (Debug `elem` opts) $ forkIO (browser baseurl) >> return ()
     server baseurl port opts args j

server :: String -> Int -> [Opt] -> [String] -> Journal -> IO ()
server baseurl port opts args j = do
  printf "Starting http server on port %d with base url %s\n" port baseurl
  withStore "hledger" $ do
    putValue "hledger" "journal" j
    withApp App{
              -- appConnPool=Nothing
              appRoot=baseurl
             ,appDataDir=datadir
             ,appStatic=fileLookupDir datadir $ typeByExt -- ++[("hamlet","text/plain")]
             ,appOpts=opts
             ,appArgs=args
             ,appJournal=j
             } $ run port

browser :: String -> IO ()
browser baseurl = do
  threadDelay $ fromIntegral browserstartdelay
  putStrLn "Attempting to start a web browser"
  openBrowserOn baseurl >> return ()

