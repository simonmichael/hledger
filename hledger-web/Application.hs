{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Data.IORef
import Import
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.HTTP.Conduit (newManager)
import Network.HTTP.Client (defaultManagerSettings)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.RootR
import Handler.JournalR
import Handler.JournalEditR
import Handler.JournalEntriesR
import Handler.RegisterR

import Hledger.Web.Options (WebOpts(..), defwebopts)
import Hledger.Data (Journal, nulljournal)
import Hledger.Read (readJournalFile)
import Hledger.Utils (error')
import Hledger.Cli.Options (defcliopts, journalFilePathFromOpts)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: WebOpts -> Journal -> AppConfig DefaultEnv Extra -> IO Application
makeApplication opts j conf = do
    foundation <- makeFoundation conf
    writeIORef (appJournal foundation) j
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
    logWare | development  = logStdoutDev
            | server_ opts = logStdout
            | otherwise    = id

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager defaultManagerSettings
    s <- staticSite
    jref <- newIORef nulljournal
    return $ App conf s manager defwebopts jref

-- for yesod devel
-- uses the journal specified by the LEDGER_FILE env var, or ~/.hledger.journal
getApplicationDev :: IO (Int, Application)
getApplicationDev = do
  f <- journalFilePathFromOpts defcliopts
  j <- either error' id `fmap` readJournalFile Nothing Nothing f
  defaultDevelApp loader (makeApplication defwebopts j)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
