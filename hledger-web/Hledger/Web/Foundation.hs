{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Define the web application's foundation, in the usual Yesod style.
--   See a default Yesod app's comments for more details of each part.

module Hledger.Web.Foundation where

import Control.Applicative ((<|>))
import Control.Monad (join, when)
-- import Control.Monad.Except (runExceptT)  -- now re-exported by Hledger
import qualified Data.ByteString.Char8 as BC
import Data.Traversable (for)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Network.HTTP.Conduit (Manager)
import Network.HTTP.Types (status403)
import Network.Wai (requestHeaders)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing,
                         getXdgDirectory)
import System.FilePath (takeFileName, (</>))
import Text.Blaze (Markup)
import Text.Hamlet (hamletFile)
import Yesod
import Yesod.Static
import Yesod.Default.Config

#ifndef DEVELOPMENT
import Hledger.Web.Settings (staticDir)
import Text.Jasmine (minifym)
import Yesod.Default.Util (addStaticContentExternal)
#endif

import Hledger
import Hledger.Cli (CliOpts(..), journalReloadIfChanged)
import Hledger.Web.Settings (Extra(..), widgetFile)
import Hledger.Web.Settings.StaticFiles
import Hledger.Web.WebOptions
import Hledger.Web.Widget.Common (balanceReportAsHtml)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , httpManager :: Manager
      --
    , appOpts    :: WebOpts
    , appJournal :: IORef Journal
        -- ^ the current journal, filtered by the initial command line query
        --   but ignoring any depth limit.
    }


-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
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
mkYesodData "App" $(parseRoutesFile "config/routes")
-- ^ defines things like:
-- type Handler = HandlerFor App   -- HandlerT App IO, https://www.yesodweb.com/book/routing-and-handlers#routing-and-handlers_handler_monad
-- type Widget = WidgetFor App ()  -- WidgetT App IO (), https://www.yesodweb.com/book/widgets

type AppRoute = Route App
type Form a = Html -> MForm Handler (FormResult a, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  approot = guessApprootOr (ApprootMaster $ appRoot . settings)

  makeSessionBackend _ = do
    hledgerdata <- getXdgDirectory XdgCache "hledger"
    createDirectoryIfMissing True hledgerdata
    let sessionexpirysecs = 120
    Just <$> defaultClientSessionBackend sessionexpirysecs (hledgerdata </> "hledger-web_client_session_key.aes")

  -- defaultLayout :: WidgetFor site () -> HandlerFor site Html
  defaultLayout widget = do

    -- Don't run if server-side UI is disabled.
    -- This single check probably covers all the HTML-returning handlers,
    -- but for now they do the check as well.
    checkServerSideUiEnabled

    master <- getYesod
    here <- fromMaybe RootR <$> getCurrentRoute
    VD{opts, j, qparam, q, qopts, caps} <- getViewData
    msg <- getMessage
    showSidebar <- shouldShowSidebar

    let rspec = reportspec_ (cliopts_ opts)
        ropts = _rsReportOpts rspec
        ropts' = (_rsReportOpts rspec)
          {accountlistmode_ = ALTree  -- force tree mode for sidebar
          ,empty_           = True    -- show zero items by default
          }
        rspec' = rspec{_rsQuery=q,_rsReportOpts=ropts'}

    hideEmptyAccts <- if empty_ ropts
                         then return True
                         else (== Just "1") . lookup "hideemptyaccts" . reqCookies <$> getRequest

    let accounts =
          balanceReportAsHtml (JournalR, RegisterR) here hideEmptyAccts j qparam qopts $
          balanceReport rspec' j

        topShowmd = if showSidebar then "col-md-4" else "col-any-0" :: Text
        topShowsm = if showSidebar then "col-sm-4" else "" :: Text
        sideShowmd = if showSidebar then "col-md-4" else "col-any-0" :: Text
        sideShowsm = if showSidebar then "col-sm-4" else "" :: Text
        mainShowmd = if showSidebar then "col-md-8" else "col-md-12" :: Text
        mainShowsm = if showSidebar then "col-sm-8" else "col-sm-12" :: Text

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.
    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_bootstrap_min_css
      addStylesheet $ StaticR css_bootstrap_datepicker_standalone_min_css
      -- load these things early, in HEAD:
      toWidgetHead [hamlet|
        <script type="text/javascript" src="@{StaticR js_jquery_min_js}">
        <script type="text/javascript" src="@{StaticR js_typeahead_bundle_min_js}">
      |]
      addScript $ StaticR js_bootstrap_min_js
      addScript $ StaticR js_bootstrap_datepicker_min_js
      addScript $ StaticR js_jquery_url_js
      addScript $ StaticR js_jquery_cookie_js
      addScript $ StaticR js_jquery_hotkeys_js
      addScript $ StaticR js_jquery_flot_min_js
      addScript $ StaticR js_jquery_flot_selection_min_js
      addScript $ StaticR js_jquery_flot_time_min_js
      addScript $ StaticR js_jquery_flot_tooltip_min_js
      toWidget [hamlet| \<!--[if lte IE 8]> <script type="text/javascript" src="@{StaticR js_excanvas_min_js}"></script> <![endif]--> |]
      addStylesheet $ StaticR hledger_css
      addScript $ StaticR hledger_js
      $(widgetFile "default-layout")

    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

#ifndef DEVELOPMENT
  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent = addStaticContentExternal minifym base64md5 staticDir (StaticR . flip StaticRoute [])
#endif

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


----------------------------------------------------------------------
-- template and handler utilities

-- view data, used by the add form and handlers
-- XXX Parameter p - show/hide postings

-- | A bundle of data useful for hledger-web request handlers and templates.
data ViewData = VD
  { opts  :: WebOpts    -- ^ the command-line options at startup
  , today :: Day        -- ^ today's date (for queries containing relative dates)
  , j     :: Journal    -- ^ the up-to-date parsed unfiltered journal
  , qparam :: Text       -- ^ the current "q" request parameter
  , q     :: Query      -- ^ a query parsed from the q parameter
  , qopts :: [QueryOpt] -- ^ query options parsed from the q parameter
  , caps  :: [Capability] -- ^ capabilities enabled for this request
  } deriving (Show)

instance Show Text.Blaze.Markup where show _ = "<blaze markup>"

-- | Gather data used by handlers and templates in the current request.
getViewData :: Handler ViewData
getViewData = do
  App{
    appOpts=opts@WebOpts{ cliopts_=copts@CliOpts{ reportspec_=rspec@ReportSpec{_rsReportOpts, _rsQuery} } },
    appJournal
  } <- getYesod
  let today = _rsDay rspec

  -- try to read the latest journal content, keeping the old content
  -- if there's an error
  (j, mjerr) <- getCurrentJournal
                appJournal
                copts{reportspec_=rspec{_rsReportOpts=_rsReportOpts{no_elide_=True}}}
                today

  -- Get the query specified by the q request parameter, or no query if this fails.
  qparam <- fromMaybe "" <$> lookupGetParam "q"
  (q1, qopts, mqerr) <- do
    case parseQuery today qparam of
      Right (q0, qopts) -> return (q0, qopts, Nothing)
      Left err         -> return (Any, [], Just err)
  -- To this, add any depth limit from the initial startup query, preserving that.
  let
    initialdepthq = filterQuery queryIsDepth _rsQuery
    q = simplifyQuery $ And [q1, initialdepthq]

  -- if either of the above gave an error, display it
  maybe (pure ()) (setMessage . toHtml) $ mjerr <|> mqerr

  -- do some permissions checking
  caps <- case capabilitiesHeader_ opts of
    Nothing -> return (capabilities_ opts)
    Just h -> do
      hs <- fmap (BC.split ',' . snd) . filter ((== h) . fst) . requestHeaders <$> waiRequest
      fmap join . for (join hs) $ \x -> case capabilityFromBS x of
        Left e -> [] <$ addMessage "" ("Unknown permission: " <> toHtml (BC.unpack e))
        Right c -> pure [c]

  return VD{opts, today, j, qparam, q, qopts, caps}

checkServerSideUiEnabled :: Handler ()
checkServerSideUiEnabled = do
  VD{opts=WebOpts{serve_api_}} <- getViewData
  when serve_api_ $
    -- this one gives 500 internal server error when called from defaultLayout:
    --  permissionDenied "server-side UI is disabled due to --serve-api"
    sendResponseStatus status403 ("server-side UI is disabled due to --serve-api" :: Text)

-- | Find out if the sidebar should be visible. Show it, unless there is a
-- showsidebar cookie set to "0", or a ?sidebar=0 query parameter.
shouldShowSidebar :: Handler Bool
shouldShowSidebar = do
  msidebarparam <- lookupGetParam "sidebar"
  msidebarcookie <- lookup "showsidebar" . reqCookies <$> getRequest
  return $
    let disablevalues = ["","0"]
    in maybe True (`notElem` disablevalues) $ msidebarparam <|> msidebarcookie

-- | Update our copy of the journal if the file changed. If there is an
-- error while reloading, keep the old one and return the error, and set a
-- ui message.
getCurrentJournal :: IORef Journal -> CliOpts -> Day -> Handler (Journal, Maybe String)
getCurrentJournal jref opts d = do
  -- re-apply any initial filter specified at startup
  let depthlessinitialq = filterQuery (not . queryIsDepth) $ _rsQuery $ reportspec_ opts
  -- XXX put this inside atomicModifyIORef' for thread safety
  j <- liftIO (readIORef jref)
  ej <- liftIO . runExceptT $ journalReloadIfChanged opts d j
  case ej of
    Left e -> do
      setMessage "error while reading journal"
      return (j, Just e)
    Right (j', True) -> do
      liftIO . writeIORef jref $ filterJournalTransactions depthlessinitialq j'
      return (j',Nothing)
    Right (_, False) -> return (j, Nothing)
