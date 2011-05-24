{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module App
    ( App (..)
    , AppRoute (..)
    , resourcesApp
    , Handler
    , Widget
    , module Yesod.Core
    , module Settings
    , StaticRoute (..)
    , lift
    , liftIO
    ) where

import Yesod.Core
import Yesod.Helpers.Static
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

import Hledger.Cli.Options (Opt)
import Hledger.Data (Journal)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    {getStatic :: Static -- ^ Settings for static file serving.
    ,appRoot    :: T.Text
    ,appOpts    :: [Opt]
    ,appArgs    :: [String]
    ,appJournal :: Journal
    }

-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler App App

-- | A useful synonym; most of the widgets functions in your application
-- will need to be of this type.
type Widget = GWidget App App

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Controller.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = appRoot

    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            widget
            addCassius $(Settings.cassiusFile "default-layout")
        hamletToRepHtml $(Settings.hamletFile "default-layout")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    -- urlRenderOverride a (StaticR s) =
    --     Just $ uncurry (joinPath a Settings.staticroot) $ renderRoute s
    -- urlRenderOverride _ _ = Nothing

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : T.unpack ext'
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", T.pack fn] [], [])