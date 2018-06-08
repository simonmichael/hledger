{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, OverloadedStrings, TemplateHaskell #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import

import Data.Default (def)
import Data.IORef (newIORef, writeIORef)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit (newManager)
import Yesod.Default.Config
import Yesod.Default.Main (defaultDevelApp)
import Yesod.Default.Handlers (getFaviconR, getRobotsR)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.RootR (getRootR)
import Handler.JournalR (getJournalR, postJournalR)
import Handler.RegisterR (getRegisterR, postRegisterR)
import Handler.SidebarR (getSidebarR)

import Hledger.Data (Journal, nulljournal)
import Hledger.Read (readJournalFile)
import Hledger.Utils (error')
import Hledger.Cli.CliOptions (defcliopts, journalFilePathFromOpts)
import Hledger.Web.WebOptions (WebOpts(..), defwebopts)

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
    foundation <- makeFoundation conf opts
    writeIORef (appJournal foundation) j
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
    logWare | development  = logStdoutDev
            | serve_ opts  = logStdout
            | otherwise    = id

makeFoundation :: AppConfig DefaultEnv Extra -> WebOpts -> IO App
makeFoundation conf opts = do
    manager <- newManager defaultManagerSettings
    s <- staticSite
    jref <- newIORef nulljournal
    return $ App conf s manager opts jref

-- for yesod devel
-- uses the journal specified by the LEDGER_FILE env var, or ~/.hledger.journal
getApplicationDev :: IO (Int, Application)
getApplicationDev = do
  f <- head `fmap` journalFilePathFromOpts defcliopts -- XXX head should be safe for now
  j <- either error' id `fmap` readJournalFile def f
  defaultDevelApp loader (makeApplication defwebopts j)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
