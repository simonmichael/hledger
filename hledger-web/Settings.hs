{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the hledger-web.hs file.
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , luciusFile
    , widgetFile
    , datadir
    , staticdir
    -- , staticroot
    , defhost
    , defport
    , defapproot
    -- , browserstartdelay
    , hledgerorgurl
    , manualurl
    ) where

import Data.Monoid (mempty) --, mappend)
import Data.Text (Text,pack)
import Language.Haskell.TH.Syntax
import System.Directory (doesFileExist)
import Text.Printf (printf)
import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import qualified Text.Julius as H
import qualified Text.Lucius as H
import Yesod.Widget (addWidget, addCassius, addJulius, addLucius)


-- browserstartdelay = 100000 -- microseconds

hledgerorgurl, manualurl :: String
hledgerorgurl     = "http://hledger.org"
manualurl         = hledgerorgurl++"/MANUAL.html"

-- | The default TCP port to listen on. May be overridden with --port.
defport :: Int
defport = 5000

defhost :: String
defhost = "localhost"

-- | The default base URL for your application. This will usually be different for
-- development and production. Yesod automatically constructs URLs for you,
-- so this value must be accurate to create valid links.
-- For hledger-web this is usually overridden with --base-url.
defapproot :: Text
defapproot = pack $ printf "http://%s:%d" defhost defport
-- #ifdef PRODUCTION
-- #else
-- #endif

-- | Hard-coded data directory path. This must be in your current dir when
-- you compile. At run time it's also required but we'll auto-create it.
datadir :: FilePath
datadir = "./.hledger/web/"

-- -- | The base URL for your static files. As you can see by the default
-- -- value, this can simply be "static" appended to your application root.
-- -- A powerful optimization can be serving static files from a separate
-- -- domain name. This allows you to use a web server optimized for static
-- -- files, more easily set expires and cache values, and avoid possibly
-- -- costly transference of cookies on static files. For more information,
-- -- please see:
-- --   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
-- --
-- -- If you change the resource pattern for StaticR in hledger-web.hs, you will
-- -- have to make a corresponding change here.
-- --
-- -- To see how this value is used, see urlRenderOverride in hledger-web.hs
-- staticroot :: Text
-- staticroot = defapproot `mappend` "/static"

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticdir :: FilePath
staticdir = datadir++"static"



-- The rest of this file contains settings which rarely need changing by a
-- user.

-- The following three functions are used for calling HTML, CSS and
-- Javascript templates from your Haskell code. During development,
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

toHamletFile, toCassiusFile, toJuliusFile, toLuciusFile :: String -> FilePath
toHamletFile x  = datadir++"templates/" ++ x ++ ".hamlet"
toCassiusFile x = datadir++"templates/" ++ x ++ ".cassius"
toJuliusFile x  = datadir++"templates/" ++ x ++ ".julius"
toLuciusFile x  = datadir++"templates/" ++ x ++ ".lucius"

hamletFile :: FilePath -> Q Exp
hamletFile = H.hamletFile . toHamletFile -- debug variant not used, http://www.yesodweb.com/book/faq#q1

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile = H.cassiusFile . toCassiusFile
#else
cassiusFile = H.cassiusFileDebug . toCassiusFile
#endif

luciusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
luciusFile = H.luciusFile . toLuciusFile
#else
luciusFile = H.luciusFileDebug . toLuciusFile
#endif

juliusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
juliusFile = H.juliusFile . toJuliusFile
#else
juliusFile = H.juliusFileDebug . toJuliusFile
#endif

widgetFile :: FilePath -> Q Exp
widgetFile x = do
    let h = unlessExists toHamletFile hamletFile
    let c = unlessExists toCassiusFile cassiusFile
    let j = unlessExists toJuliusFile juliusFile
    let l = unlessExists toLuciusFile luciusFile
    [|addWidget $h >> addCassius $c >> addJulius $j >> addLucius $l|]
  where
    unlessExists tofn f = do
        e <- qRunIO $ doesFileExist $ tofn x
        if e then f x else [|mempty|]
