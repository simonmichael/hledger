{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Hledger.Web.Application
  ( makeApplication
  , makeFoundation
  ) where

import Data.IORef (newIORef, writeIORef)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai.Middleware.Cors
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit (newManager)
import Yesod.Default.Config

import Hledger.Data (Journal, nulljournal)

import Hledger.Web.Handler.AddR
import Hledger.Web.Handler.MiscR
import Hledger.Web.Handler.EditR
import Hledger.Web.Handler.UploadR
import Hledger.Web.Handler.JournalR
import Hledger.Web.Handler.RegisterR
import Hledger.Web.Import
import Hledger.Web.WebOptions (WebOpts(serve_,serve_api_,cors_))

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: WebOpts -> Journal -> AppConfig DefaultEnv Extra -> IO Application
makeApplication opts' j' conf' = do
    foundation <- makeFoundation conf' opts'
    writeIORef (appJournal foundation) j'
    (logWare . corsWare) <$> toWaiApp foundation
  where
    logWare | development  = logStdoutDev
            | serve_ opts' || serve_api_ opts' = logStdout
            | otherwise    = id
    corsWare | cors_ opts' = simpleCors
             | otherwise = id

makeFoundation :: AppConfig DefaultEnv Extra -> WebOpts -> IO App
makeFoundation conf opts' = do
    manager <- newManager defaultManagerSettings
    s <- staticSite
    jref <- newIORef nulljournal
    return $ App conf s manager opts' jref
