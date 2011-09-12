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
import System.Exit
import System.IO.Storage (withStore, putValue)
import Text.Printf
#ifndef PRODUCTION
import Network.Wai.Middleware.Debug (debugHandle)
import Yesod.Logger (logString, logLazyText, flushLogger, makeLogger)
#else
import Yesod.Logger (makeLogger)
#endif

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
  putStrLn $ "Running self-tests..."
  runTestsOrExit $ cliopts_ opts
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

    -- yesod main
    logger <- makeLogger
    -- args   <- cmdArgs argConfig
    -- env    <- getAppEnv args
    let env = Development
    -- c <- loadConfig env
    -- let c' = if port_ opts /= 0
    --         then c{ appPort = port args }
    --         else c
    let c = AppConfig {
              appEnv = env
            , appPort = port_ opts
            , appRoot = pack baseurl
            }

#if PRODUCTION
    withApp c logger $ run (appPort c)
#else
    logString logger $ (show env) ++ " application launched, listening on port " ++ show (appPort c)
    withApp c logger $ run (appPort c) . debugHandle (logHandle logger)
    flushLogger logger

    where
        logHandle logger msg = logLazyText logger msg >> flushLogger logger
#endif

-- data ArgConfig = ArgConfig
--     { environment :: String
--     , port        :: Int
--     } deriving (Show, Data, Typeable)

-- argConfig :: ArgConfig
-- argConfig = ArgConfig
--     { environment = def 
--         &= help ("application environment, one of: " ++ (foldl1 (\a b -> a ++ ", " ++ b) environments))
--         &= typ "ENVIRONMENT"
--     , port = def
--         &= typ "PORT"
--     }

-- environments :: [String]
-- environments = map ((map toLower) . show) ([minBound..maxBound] :: [AppEnvironment])

-- | retrieve the -e environment option
-- getAppEnv :: ArgConfig -> IO AppEnvironment
-- getAppEnv cfg = do
--     let e = if environment cfg /= ""
--             then environment cfg
--             else
-- #if PRODUCTION
--                 "production"
-- #else
--                 "development"
-- #endif
--     return $ read $ capitalize e

--     where
--         capitalize [] = []
--         capitalize (x:xs) = toUpper x : map toLower xs
