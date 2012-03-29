{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Hledger.Web.Foundation
    ( App (..)
    , Route (..)
    -- , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , module Yesod.Core
    , liftIO
    ) where

import Prelude
import Yesod.Core hiding (Route)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Static
import Yesod.Logger (Logger, logMsg, formatLogText)
import Control.Monad.IO.Class (liftIO)
import Web.ClientSession (getKey)
import Text.Hamlet

import Hledger.Web.Options
import qualified Hledger.Web.Settings
import Hledger.Web.Settings (Extra (..))
import Hledger.Web.Settings.StaticFiles


-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.

    ,appOpts    :: WebOpts
    -- ,appJournal :: Journal
    }

-- Set up i18n messages. See the message folder.
-- mkMessage "App" "messages" "en"

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
    -- approot = Hledger.Web.Settings.appRoot . settings
    approot = ApprootMaster $ appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "client_session_key.aes"

    defaultLayout widget = do
        -- master <- getYesod
        -- mmsg <- getMessage
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        -- pc <- widgetToPageContent $ do
        --     $(widgetFile "normalize")
        --     $(widgetFile "default-layout")
        -- hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")
        pc <- widgetToPageContent $ do
          widget
        hamletToRepHtml [hamlet|
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
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal (const $ Left ()) base64md5 Hledger.Web.Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody
