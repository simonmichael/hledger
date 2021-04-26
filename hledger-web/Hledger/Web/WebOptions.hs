{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hledger.Web.WebOptions where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.UTF8 (fromString)
import Data.CaseInsensitive (CI, mk)
import Control.Monad (join)
import Data.Default (Default(def))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import System.Environment (getArgs)
import Network.Wai as WAI
import Network.Wai.Middleware.Cors

import Hledger.Cli hiding (progname, version)
import Hledger.Web.Settings (defhost, defport, defbaseurl)

progname, version :: String
progname = "hledger-web"
#ifdef VERSION
version = VERSION
#else
version = ""
#endif
prognameandversion :: String
prognameandversion = versiondescription progname

webflags :: [Flag RawOpts]
webflags =
  [ flagNone
      ["serve", "server"]
      (setboolopt "serve")
      "serve and log requests, don't browse or auto-exit"
  , flagNone
      ["serve-api"]
      (setboolopt "serve-api")
      "like --serve, but serve only the JSON web API, without the server-side web UI"
  , flagReq
      ["cors"]
      (\s opts -> Right $ setopt "cors" s opts)
      "ORIGIN"
      ("allow cross-origin requests from the specified origin; setting ORIGIN to \"*\" allows requests from any origin")
  , flagReq
      ["socket"]
      (\s opts -> Right $ setopt "socket" s opts)
      "SOCKET"
      "use the given socket instead of the given IP and port (implies --serve)"
  , flagReq
      ["host"]
      (\s opts -> Right $ setopt "host" s opts)
      "IPADDR"
      ("listen on this IP address (default: " ++ defhost ++ ")")
  , flagReq
      ["port"]
      (\s opts -> Right $ setopt "port" s opts)
      "PORT"
      ("listen on this TCP port (default: " ++ show defport ++ ")")
  , flagReq
      ["base-url"]
      (\s opts -> Right $ setopt "base-url" s opts)
      "BASEURL"
      "set the base url (default: http://IPADDR:PORT)"
  , flagReq
      ["file-url"]
      (\s opts -> Right $ setopt "file-url" s opts)
      "FILEURL"
      "set the static files url (default: BASEURL/static)"
  , flagReq
      ["capabilities"]
      (\s opts -> Right $ setopt "capabilities" s opts)
      "CAP[,CAP..]"
      "enable the view, add, and/or manage capabilities (default: view,add)"
  , flagReq
      ["capabilities-header"]
      (\s opts -> Right $ setopt "capabilities-header" s opts)
      "HTTPHEADER"
      "read capabilities to enable from a HTTP header, like X-Sandstorm-Permissions (default: disabled)"
  , flagNone
      ["test"]
      (setboolopt "test")
      "run hledger-web's tests and exit. hspec test runner args may follow a --, eg: hledger-web --test -- --help"
  ]

webmode :: Mode RawOpts
webmode =
  (mode
     "hledger-web"
     (setopt "command" "web" def)
     "start serving the hledger web interface"
     (argsFlag "[PATTERNS]")
     [])
  { modeGroupFlags =
      Group
      { groupUnnamed = webflags
      , groupHidden =
          hiddenflags ++
          [ flagNone
              ["binary-filename"]
              (setboolopt "binary-filename")
              "show the download filename for this executable, and exit"
          ]
      , groupNamed = [generalflagsgroup1]
      }
  , modeHelpSuffix = []
  }

-- hledger-web options, used in hledger-web and above
data WebOpts = WebOpts
  { serve_ :: Bool
  , serve_api_ :: Bool
  , cors_ :: Maybe String
  , host_ :: String
  , port_ :: Int
  , base_url_ :: String
  , file_url_ :: Maybe String
  , capabilities_ :: [Capability]
  , capabilitiesHeader_ :: Maybe (CI ByteString)
  , cliopts_ :: CliOpts
  , socket_ :: Maybe String
  } deriving (Show)

defwebopts :: WebOpts
defwebopts = WebOpts
  { serve_              = False
  , serve_api_          = False
  , cors_               = Nothing
  , host_               = ""
  , port_               = def
  , base_url_           = ""
  , file_url_           = Nothing
  , capabilities_       = [CapView, CapAdd]
  , capabilitiesHeader_ = Nothing
  , cliopts_            = def
  , socket_             = Nothing
  }

instance Default WebOpts where def = defwebopts

rawOptsToWebOpts :: RawOpts -> IO WebOpts
rawOptsToWebOpts rawopts =
  checkWebOpts <$> do
    cliopts <- rawOptsToCliOpts rawopts
    let h = fromMaybe defhost $ maybestringopt "host" rawopts
        p = fromMaybe defport $ maybeposintopt "port" rawopts
        b =
          maybe (defbaseurl h p) stripTrailingSlash $
          maybestringopt "base-url" rawopts
        caps' = join $ T.splitOn "," . T.pack <$> listofstringopt "capabilities" rawopts
        caps = case traverse capabilityFromText caps' of
          Left e -> error' ("Unknown capability: " ++ T.unpack e)  -- PARTIAL:
          Right [] -> [CapView, CapAdd]
          Right xs -> xs
        sock = stripTrailingSlash <$> maybestringopt "socket" rawopts
    return
      defwebopts
      { serve_ = case sock of
          Just _ -> True
          Nothing -> boolopt "serve" rawopts
      , serve_api_ = boolopt "serve-api" rawopts
      , cors_ = maybestringopt "cors" rawopts
      , host_ = h
      , port_ = p
      , base_url_ = b
      , file_url_ = stripTrailingSlash <$> maybestringopt "file-url" rawopts
      , capabilities_ = caps
      , capabilitiesHeader_ = mk . BC.pack <$> maybestringopt "capabilities-header" rawopts
      , cliopts_ = cliopts
      , socket_ = sock
      }
  where
    stripTrailingSlash = reverse . dropWhile (== '/') . reverse -- yesod don't like it

checkWebOpts :: WebOpts -> WebOpts
checkWebOpts = id

getHledgerWebOpts :: IO WebOpts
getHledgerWebOpts = do
  args <- fmap replaceNumericFlags . expandArgsAt =<< getArgs
  rawOptsToWebOpts . either usageError id $ process webmode args

data Capability
  = CapView
  | CapAdd
  | CapManage
  deriving (Eq, Ord, Bounded, Enum, Show)

capabilityFromText :: Text -> Either Text Capability
capabilityFromText "view" = Right CapView
capabilityFromText "add" = Right CapAdd
capabilityFromText "manage" = Right CapManage
capabilityFromText x = Left x

capabilityFromBS :: ByteString -> Either ByteString Capability
capabilityFromBS "view" = Right CapView
capabilityFromBS "add" = Right CapAdd
capabilityFromBS "manage" = Right CapManage
capabilityFromBS x = Left x

simplePolicyWithOrigin :: Origin -> CorsResourcePolicy
simplePolicyWithOrigin origin =
    simpleCorsResourcePolicy { corsOrigins = Just ([origin], False) }


corsPolicyFromString :: String -> WAI.Middleware
corsPolicyFromString origin =
  let
    policy = case origin of
        "*" -> simpleCorsResourcePolicy
        url -> simplePolicyWithOrigin $ fromString url
  in
    cors (const $ Just policy)

corsPolicy :: WebOpts -> (Application -> Application)
corsPolicy opts =
  maybe id corsPolicyFromString $ cors_ opts
