{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module AppRun
    ( withApp
    , withDevelApp
    ) where

import App
import Settings
import Yesod.Helpers.Static
import Data.ByteString (ByteString)
import Network.Wai (Application)
import Data.Dynamic (Dynamic, toDyn)
import System.FilePath ((</>))

-- Import all relevant handler modules here.
import Handlers

import Hledger.Data (nulljournal)

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in App.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" $ Settings.staticdir </> "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withApp :: App -> (Application -> IO a) -> IO a
withApp a f = do
    toWaiApp a >>= f
  -- where
  --   s = static Settings.staticdir

withDevelApp :: Dynamic
-- withDevelApp = undefined
withDevelApp = toDyn (withApp a :: (Application -> IO ()) -> IO ())
   where a = App{
              getStatic=static Settings.staticdir
             ,appRoot=Settings.defapproot
             ,appOpts=[]
             ,appArgs=[]
             ,appJournal=nulljournal
             }

