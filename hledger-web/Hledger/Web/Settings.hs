{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes  #-}
{-# LANGUAGE OverloadedStrings #-}
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

    -- , hamletFile
    -- , cassiusFile
    -- , juliusFile
    -- , luciusFile
    -- , AppEnvironment(..)
    -- , AppConfig(..)
    , defport
    , defbaseurl
    , hledgerorgurl
    , manualurl

    ) where

import Prelude
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Yesod.Default.Config
import qualified Yesod.Default.Util
import Data.Text (Text)
import Data.Yaml
import Control.Applicative

-- import qualified Text.Hamlet as S
-- import qualified Text.Cassius as S
-- import qualified Text.Julius as S
-- import qualified Text.Lucius as S
import Text.Printf
import qualified Text.Shakespeare.Text as S
import Text.Shakespeare.Text (st)
import Yesod.Widget (addWidget, addCassius, addJulius, addLucius, whamletFile)
import Data.Monoid (mempty)
import System.Directory (doesFileExist)
import Data.Text (pack)
import Control.Monad (join)


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

{-
-- The rest of this file contains settings which rarely need changing by a
-- user.

-- The following functions are used for calling HTML, CSS,
-- Javascript, and plain text templates from your Haskell code. During development,
-- the "Debug" versions of these functions are used so that changes to
-- the templates are immediately reflected in an already running
-- application. When making a production compile, the non-debug version
-- is used for increased performance.
--
-- You can see an example of how to call these functions in Handler/Root.hs
--
-- Note: due to polymorphic Hamlet templates, hamletFileDebug is no longer
-- used; to get the same auto-loading effect, it is recommended that you
-- use the devel server.

-- | expects a root folder for each type, e.g: hamlet/ lucius/ julius/
globFile :: String -> String -> FilePath
-- globFile kind x = kind ++ "/" ++ x ++ "." ++ kind
globFile kind x = "templates/" ++ x ++ "." ++ kind

hamletFile :: FilePath -> Q Exp
hamletFile = S.hamletFile . globFile "hamlet"

cassiusFile :: FilePath -> Q Exp
cassiusFile =
#ifdef PRODUCTION
  S.cassiusFile . globFile "cassius"
#else
  S.cassiusFileDebug . globFile "cassius"
#endif

luciusFile :: FilePath -> Q Exp
luciusFile =
#ifdef PRODUCTION
  S.luciusFile . globFile "lucius"
#else
  S.luciusFileDebug . globFile "lucius"
#endif

juliusFile :: FilePath -> Q Exp
juliusFile =
#ifdef PRODUCTION
  S.juliusFile . globFile "julius"
#else
  S.juliusFileDebug . globFile "julius"
#endif

textFile :: FilePath -> Q Exp
textFile =
#ifdef PRODUCTION
  S.textFile . globFile "text"
#else
  S.textFileDebug . globFile "text"
#endif

widgetFile :: FilePath -> Q Exp
widgetFile x = do
    let h = whenExists (globFile "hamlet")  (whamletFile . globFile "hamlet")
    let c = whenExists (globFile "cassius") cassiusFile
    let j = whenExists (globFile "julius")  juliusFile
    let l = whenExists (globFile "lucius")  luciusFile
    [|addWidget $h >> addCassius $c >> addJulius $j >> addLucius $l|]
  where
    whenExists tofn f = do
        e <- qRunIO $ doesFileExist $ tofn x
        if e then f x else [|mempty|]
-}