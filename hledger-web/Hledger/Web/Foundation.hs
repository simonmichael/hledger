{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-

Define the web application's foundation, in the usual Yesod style.
See a default Yesod app's comments for more details of each part.

-}

module Hledger.Web.Foundation
    ( App (..)
    , Route (..)
    , AppRoute
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

-- | The web application's configuration and data, available to all request handlers.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , appOpts    :: WebOpts
    -- , appJournal :: Journal
    }

-- Set up i18n messages.
-- mkMessage "App" "messages" "en"

-- The web application's routes (urls).
mkYesodData "App" $(parseRoutesFile "routes")

-- | A convenience alias.
type AppRoute = Route App

-- More configuration, including the default page layout.
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
