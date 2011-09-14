{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hledger.Web.Foundation
    ( App (..)
    , AppRoute (..)
    , resourcesApp
    , Handler
    , Widget
    , module Yesod.Core
    -- , module Settings
    , StaticRoute (..)
    , lift
    , liftIO
    ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import System.Directory
import Text.Hamlet hiding (hamletFile)
import Web.ClientSession (getKey)
import Yesod.Core
import Yesod.Logger (Logger, logLazyText)
import Yesod.Static (Static, base64md5, StaticRoute(..))
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Hledger.Web.Options
import Hledger.Web.Settings
import Hledger.Web.Settings.StaticFiles


-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: Hledger.Web.Settings.AppConfig
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.

    ,appOpts    :: WebOpts
    -- ,appJournal :: Journal
    }

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
--   resources declared below. This is used in Handler.hs by the call to
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
    approot = Hledger.Web.Settings.appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "client_session_key.aes"

    defaultLayout widget = do
        -- mmsg <- getMessage
        pc <- widgetToPageContent $ do
            widget
        --     addCassius $(cassiusFile "default-layout")
        -- hamletToRepHtml $(hamletFile "default-layout")
        hamletToRepHtml [$hamlet|
!!!
<html
 <head
  <title>#{pageTitle pc}
  ^{pageHead pc}
  <meta http-equiv=Content-Type content="text/html; charset=utf-8"
  <script type=text/javascript src=@{StaticR jquery_js}
  <script type=text/javascript src=@{StaticR jquery_url_js}
  <script type=text/javascript src=@{StaticR jquery_flot_js}
  <!--[if lte IE 8]><script language="javascript" type="text/javascript" src="excanvas.min.js"></script><![endif]-->
  <script type=text/javascript src=@{StaticR dhtmlxcommon_js}
  <script type=text/javascript src=@{StaticR dhtmlxcombo_js}
  <script type=text/javascript src=@{StaticR hledger_js}
  <link rel=stylesheet type=text/css media=all href=@{StaticR style_css}
 <body
  ^{pageBody pc}
|]

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    -- urlRenderOverride y (StaticR s) =
    --     Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    -- urlRenderOverride _ _ = Nothing

    messageLogger y loc level msg =
      formatLogMessage loc level msg >>= logLazyText (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : T.unpack ext'
        let statictmp = Hledger.Web.Settings.staticDir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", T.pack fn] [], [])
