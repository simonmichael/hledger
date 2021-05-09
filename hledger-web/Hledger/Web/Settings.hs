{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Hledger.Web.Settings where

import Data.Default (def)
import Data.Text (Text)
import Data.Yaml
import Language.Haskell.TH.Syntax (Q, Exp)
import Text.Hamlet
import Text.Shakespeare.Text (st)
import Yesod.Default.Config
import Yesod.Default.Util

development :: Bool
development =
#if DEVELOPMENT
  True
#else
  False
#endif

production :: Bool
production = not development

hledgerorgurl :: Text
hledgerorgurl = "http://hledger.org"

manualurl :: Text
manualurl = hledgerorgurl <> "/manual"

-- | The default IP address to listen on. May be overridden with --host.
defhost :: String
defhost = "127.0.0.1"

-- | The default TCP port to listen on. May be overridden with --port.
defport :: Int
defport = 5000

defbaseurl :: String -> Int -> String
defbaseurl host port =
  if ':' `elem` host then
    "http://[" ++ host ++ "]" ++ if port /= 80 then ":" ++ show port else ""
  else
    "http://" ++ host ++ if port /= 80 then ":" ++ show port else ""

-- Static setting below. Changing these requires a recompile

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
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppConfig DefaultEnv Extra -> Text
staticRoot conf = case extraStaticRoot $ appExtra conf of
                    Just root -> root
                    Nothing -> [st|#{appRoot conf}/static|]

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

data Extra = Extra
    { extraCopyright  :: Text
    , extraAnalytics  :: Maybe Text -- ^ Google Analytics
    , extraStaticRoot :: Maybe Text
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:? "analytics"
    <*> o .:? "staticRoot"
