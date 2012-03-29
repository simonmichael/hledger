{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the hledger-web.hs file.
module Hledger.Web.Settings
    ( widgetFile
    , staticRoot
    , staticDir
    , Extra (..)
    , parseExtra
    , defport
    , defbaseurl
    , hledgerorgurl
    , manualurl
    ) where

import Control.Applicative
import Data.Text (Text)
import Data.Yaml
import Language.Haskell.TH.Syntax
import Prelude
import Text.Printf
import Text.Shakespeare.Text (st)
import Yesod.Default.Config
import qualified Yesod.Default.Util


hledgerorgurl, manualurl :: String
hledgerorgurl     = "http://hledger.org"
manualurl         = hledgerorgurl++"/MANUAL.html"

-- | The default TCP port to listen on. May be overridden with --port.
defport :: Int
defport = 5000

defbaseurl :: Int -> String
defbaseurl port = printf "http://localhost:%d" port


-- | Dynamic per-environment configuration loaded from the YAML file Settings.yaml.
-- Use dynamic settings to avoid the need to re-compile the application (between staging and production environments).
-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in hledger-web.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in hledger-web.hs
staticRoot :: AppConfig DefaultEnv a ->  Text
staticRoot conf = [st|#{appRoot conf}/static|]

widgetFile :: String -> Q Exp
#if DEVELOPMENT
widgetFile = Yesod.Default.Util.widgetFileReload
#else
widgetFile = Yesod.Default.Util.widgetFileNoReload
#endif

data Extra = Extra
    { extraCopyright :: Text
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    }

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:? "analytics"
