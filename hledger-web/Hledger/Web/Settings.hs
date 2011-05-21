{-# LANGUAGE CPP, OverloadedStrings #-}
module Hledger.Web.Settings
    (
     hamletFile
    , cassiusFile
    , juliusFile
    -- , connStr
    -- , ConnectionPool
    -- , withConnectionPool
    -- , runConnectionPool
    , approot
    , staticroot
    , datadir
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

import Language.Haskell.TH.Syntax
import System.FilePath ((</>))
import qualified Text.Cassius as H
import qualified Text.Hamlet as H
import qualified Text.Julius as H
import Text.Printf (printf)
-- import Database.Persist.Sqlite
-- import Yesod (MonadCatchIO)
import Yesod.Helpers.Static


browserstartdelay = 100000 -- microseconds

----------------------------------------------------------------------
-- urls
----------------------------------------------------------------------

hledgerorgurl, manualurl :: String
hledgerorgurl     = "http://hledger.org"
manualurl         = hledgerorgurl++"/MANUAL.html"

defhost           = "localhost" :: String
defport           = 5000

approot :: String
#ifdef PRODUCTION
approot = printf "http://%s:%d" defhost (defport :: Int) :: String
#else
approot = printf "http://%s:%d" defhost (defport :: Int) :: String
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

-- | Hard-coded data directory path. This must be in your current dir when
-- you compile. At run time it's also required but we'll auto-create it.
datadir :: FilePath
datadir = "./.hledger/web/"

-- The following are compile-time macros. If the file paths they point to
-- don't exist, they will give an error (at compile time). If PRODUCTION
-- is defined, files are read only once at (startup?) time, otherwise
-- repeatedly at run time.

hamletFile :: FilePath -> Q Exp
#ifdef PRODUCTION
hamletFile x = H.hamletFile $ datadir </> (x ++ ".hamlet")
#else
hamletFile x = H.hamletFileDebug $ datadir </> (x ++ ".hamlet")
#endif

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile x = H.cassiusFile $ datadir </> (x ++ ".cassius")
#else
cassiusFile x = H.cassiusFileDebug $ datadir </> (x ++ ".cassius")
#endif

juliusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
juliusFile x = H.juliusFile $ datadir </> (x ++ ".julius")
#else
juliusFile x = H.juliusFileDebug $ datadir </> (x ++ ".julius")
#endif

----------------------------------------------------------------------
-- database
----------------------------------------------------------------------

-- connStr :: String
-- #ifdef PRODUCTION
-- connStr = "production.db3"
-- #else
-- connStr = "debug.db3"
-- #endif

-- connectionCount :: Int
-- connectionCount = 10

-- withConnectionPool :: MonadCatchIO m => (ConnectionPool -> m a) -> m a
-- withConnectionPool = withSqlitePool connStr connectionCount

-- runConnectionPool :: MonadCatchIO m => SqlPersist m a -> ConnectionPool -> m a
-- runConnectionPool = runSqlPool

