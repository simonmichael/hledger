{-# LANGUAGE PackageImports #-}
import "hledger-web" Hledger.Web.Application (getApplicationDev)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

-- import Network.Wai.Middleware.Debug (debugHandle)
-- import Yesod.Logger (logString, logLazyText, flushLogger, makeLogger)

main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    forkIO $ runSettings defaultSettings
        { settingsPort = port
        } app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess

--     logString logger $ (show env) ++ " application launched, listening on port " ++ show (appPort c)
--     withApp c logger opts $ run (appPort c) . debugHandle (logHandle logger)
--     flushLogger logger

--     where
--         logHandle logger msg = logLazyText logger msg >> flushLogger logger
