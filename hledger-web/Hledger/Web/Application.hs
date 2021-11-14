{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Hledger.Web.Application
  ( makeApplication
  , makeFoundationWith
  ) where

import Data.IORef (newIORef)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit (newManager)
import System.IO (stderr, hPutStrLn)
import Yesod.Default.Config

import Hledger.Cli (withJournalTry)
import Hledger.Data (Journal)

import Hledger.Web.Handler.AddR
import Hledger.Web.Handler.MiscR
import Hledger.Web.Handler.EditR
import Hledger.Web.Handler.UploadR
import Hledger.Web.Handler.JournalR
import Hledger.Web.Handler.RegisterR
import Hledger.Web.Import
import Hledger.Web.Error as WebError
import Hledger.Web.WebOptions (WebOpts(serve_,serve_api_, cliopts_), corsPolicy)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: WebOpts -> AppConfig DefaultEnv Extra -> IO Application
makeApplication opts' conf' = do
    let application = withJournalTry (toWaiApp <=< makeError) (cliopts_ opts') (toWaiApp <=< (\j -> makeFoundationWith j conf' opts'))
    (logWare . (corsPolicy opts')) <$> application
  where
    logWare | development  = logStdoutDev
            | serve_ opts' || serve_api_ opts' = logStdout
            | otherwise    = id

makeError :: String -> IO WebError.Error
makeError err = do
  hPutStrLn stderr err
  pure $ WebError.Error err

-- Make a Foundation with the given Journal as its state.
makeFoundationWith :: Journal -> AppConfig DefaultEnv Extra -> WebOpts -> IO App
makeFoundationWith j' conf opts' = do
    manager <- newManager defaultManagerSettings
    s <- staticSite
    jref <- newIORef j'
    return $ App conf s manager opts' jref
