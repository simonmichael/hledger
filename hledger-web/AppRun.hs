{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module AppRun (
               withApp
              ,withDevelApp
              )
where

import Data.Dynamic (Dynamic, toDyn)
import Network.Wai (Application)
import Yesod.Helpers.Static

import Hledger.Data (nulljournal)

import App
import Handlers
import Settings

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in App.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withApp :: App -> (Application -> IO a) -> IO a
withApp a f = toWaiApp a >>= f

withDevelApp :: Dynamic
withDevelApp = toDyn (withApp a :: (Application -> IO ()) -> IO ())
   where a = App{
              getStatic=static Settings.staticdir
             ,appRoot=Settings.defapproot
             ,appOpts=[]
             ,appArgs=[]
             ,appJournal=nulljournal
             }
