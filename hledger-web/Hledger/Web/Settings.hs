{-# LANGUAGE CPP #-}
module Hledger.Web.Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , connStr
    , ConnectionPool
    , withConnectionPool
    , runConnectionPool
    , approot
    , staticroot
    , datadir
    , staticdir
    , templatesdir

    , defhost
    , defport
    , browserstartdelay
    , hledgerorgurl
    , manualurl
    , style_css
    , hledger_js
    , jquery_js
    , jquery_url_js
    , dhtmlxcommon_js
    , dhtmlxcombo_js
    , robots_txt
    ) where

import System.FilePath ((</>))
import Text.Printf (printf)
import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import qualified Text.Julius as H
import Language.Haskell.TH.Syntax
import Database.Persist.Sqlite
import Yesod (MonadCatchIO)
import Yesod.Helpers.Static


browserstartdelay = 100000 -- microseconds

----------------------------------------------------------------------
-- urls
----------------------------------------------------------------------

hledgerorgurl     = "http://hledger.org"
manualurl         = hledgerorgurl++"/MANUAL.html"

defhost           = "localhost"
defport           = 5000

approot :: String
#ifdef PRODUCTION
approot = printf "http://%s:%d" defhost (defport :: Int)
#else
approot = printf "http://%s:%d" defhost (defport :: Int)
#endif

staticroot :: String
staticroot = approot ++ "/static"

-- Some static routes we can refer to by name, without hard-coded filesystem location.
style_css       = StaticRoute ["style.css"] []
hledger_js      = StaticRoute ["hledger.js"] []
jquery_js       = StaticRoute ["jquery.js"] []
jquery_url_js   = StaticRoute ["jquery.url.js"] []
dhtmlxcommon_js = StaticRoute ["dhtmlxcommon.js"] []
dhtmlxcombo_js  = StaticRoute ["dhtmlxcombo.js"] []

-- Content for /robots.txt
robots_txt = "User-agent: *"


----------------------------------------------------------------------
-- filesystem
----------------------------------------------------------------------

-- XXX hard-coded data directory path. This must be in your current dir
-- when you run or compile hledger-web.
datadir :: FilePath
datadir = "data"

staticdir :: FilePath
staticdir = datadir ++ "/static"

templatesdir :: FilePath
templatesdir = datadir ++ "/templates"

-- The following are compile-time macros. If the file paths they point to
-- don't exist, they will give an error (at compile time). In production
-- mode, files are read once at compile time, otherwise repeatedly at runtime.

hamletFile :: FilePath -> Q Exp
#ifdef PRODUCTION
hamletFile x = H.hamletFile $ templatesdir </> (x ++ ".hamlet")
#else
hamletFile x = H.hamletFileDebug $ templatesdir </> (x ++ ".hamlet")
#endif

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile x = H.cassiusFile $ templatesdir </> (x ++ ".cassius")
#else
cassiusFile x = H.cassiusFileDebug $ templatesdir </> (x ++ ".cassius")
#endif

juliusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
juliusFile x = H.juliusFile $ templatesdir </> (x ++ ".julius")
#else
juliusFile x = H.juliusFileDebug $ templatesdir </> (x ++ ".julius")
#endif

----------------------------------------------------------------------
-- database
----------------------------------------------------------------------

connStr :: String
#ifdef PRODUCTION
connStr = "production.db3"
#else
connStr = "debug.db3"
#endif

connectionCount :: Int
connectionCount = 10

withConnectionPool :: MonadCatchIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool connStr connectionCount

runConnectionPool :: MonadCatchIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool

