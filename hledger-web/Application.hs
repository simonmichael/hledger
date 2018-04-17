{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, OverloadedStrings, TemplateHaskell #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Data.Default
import Data.IORef
import Import
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.HTTP.Conduit (newManager)
import Prelude (head)

-- adapt to http-conduit 1.x or 2.x when cabal macros are available, otherwise assume 2.x
#ifdef MIN_VERSION_http_conduit
#if MIN_VERSION_http_conduit(2,0,0)
#define http_conduit_2
#endif
#else
#define http_conduit_2
#endif
#ifdef http_conduit_2
import Network.HTTP.Client (defaultManagerSettings)
#else
import Network.HTTP.Conduit (def)
#endif

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.RootR
import Handler.JournalR
import Handler.RegisterR
import Handler.SidebarR

import Hledger.Web.WebOptions (WebOpts(..), defwebopts)
import Hledger.Data (Journal, nulljournal)
import Hledger.Read (readJournalFile)
import Hledger.Utils (error')
import Hledger.Cli.CliOptions (defcliopts, journalFilePathFromOpts)

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
    manager <- newManager
#ifdef http_conduit_2
               defaultManagerSettings
#else
               def
#endif
    s <- staticSite
    jref <- newIORef nulljournal
    return $ App conf s manager opts jref

-- for yesod devel
-- uses the journal specified by the LEDGER_FILE env var, or ~/.hledger.journal
getApplicationDev :: IO (Int, Application)
getApplicationDev = do
  f <- head `fmap` journalFilePathFromOpts defcliopts -- XXX head should be safe for now
  j <- either error' id `fmap` readJournalFile Nothing def f
  defaultDevelApp loader (makeApplication defwebopts j)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
