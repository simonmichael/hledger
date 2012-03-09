{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hledger.Web.Application
    ( getApplication
    , getApplicationDev
    )
where

import Yesod.Default.Config
import Yesod.Default.Main (defaultDevelApp)
import Yesod.Default.Handlers (getRobotsR)
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS)
import Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback)
#endif
import Network.Wai (Application)

import Hledger.Web.Foundation
import Hledger.Web.Handlers
import Hledger.Web.Options
import Hledger.Web.Settings (parseExtra)
import Hledger.Web.Settings.StaticFiles (staticSite)

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in App.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

getApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
getApplication conf logger = do
    s <- staticSite
    let foundation = App conf setLogger s defwebopts -- XXX
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logCallbackDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger -- by default the logger is set for development
    logWare = logCallback (logBS setLogger)
#endif

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader getApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
