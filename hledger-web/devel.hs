{-# LANGUAGE PackageImports #-}
import "hledger-web" Application (getApplicationDev)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import System.IO.Storage (withStore, putValue)

import Hledger (readJournalFile)

main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    forkIO $
      withStore "hledger" $ do
        readJournalFile Nothing Nothing "dev.journal" >>= putValue "hledger" "journal"
        runSettings defaultSettings
          { settingsPort = port
          } app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "yesod-devel/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
