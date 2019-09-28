{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Hledger.Web.WebOptions where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.CaseInsensitive (CI, mk)
import Control.Monad (join)
import Data.Default (Default(def))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import System.Environment (getArgs)

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
prognameandversion = progname ++ " " ++ version :: String

webflags :: [Flag [(String, String)]]
webflags =
  [ flagNone
      ["serve", "server"]
      (setboolopt "serve")
      "serve and log requests, don't browse or auto-exit"
  , flagNone
      ["serve-api"]
      (setboolopt "serve-api")
      "like --serve, but serve only the JSON web API, without the server-side web UI"
  , flagNone
      ["cors"]
      (setboolopt "cors")
      ("allow cross-origin requests, setting the Access-Control-Allow-Origin HTTP header to *")
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
  ]

webmode :: Mode [(String, String)]
webmode =
  (mode
     "hledger-web"
     [("command", "web")]
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
  , cors_ :: Bool
  , host_ :: String
  , port_ :: Int
  , base_url_ :: String
  , file_url_ :: Maybe String
  , capabilities_ :: [Capability]
  , capabilitiesHeader_ :: Maybe (CI ByteString)
  , cliopts_ :: CliOpts
  } deriving (Show)

defwebopts :: WebOpts
defwebopts = WebOpts def def def def def def def [CapView, CapAdd] Nothing def

instance Default WebOpts where def = defwebopts

rawOptsToWebOpts :: RawOpts -> IO WebOpts
rawOptsToWebOpts rawopts =
  checkWebOpts <$> do
    cliopts <- rawOptsToCliOpts rawopts
    let h = fromMaybe defhost $ maybestringopt "host" rawopts
        p = fromMaybe defport $ maybeintopt "port" rawopts
        b =
          maybe (defbaseurl h p) stripTrailingSlash $
          maybestringopt "base-url" rawopts
        caps' = join $ T.splitOn "," . T.pack <$> listofstringopt "capabilities" rawopts
        caps = case traverse capabilityFromText caps' of
          Left e -> error' ("Unknown capability: " ++ T.unpack e)
          Right [] -> [CapView, CapAdd]
          Right xs -> xs
    return
      defwebopts
      { serve_ = boolopt "serve" rawopts
      , serve_api_ = boolopt "serve-api" rawopts
      , cors_ = boolopt "cors" rawopts
      , host_ = h
      , port_ = p
      , base_url_ = b
      , file_url_ = stripTrailingSlash <$> maybestringopt "file-url" rawopts
      , capabilities_ = caps
      , capabilitiesHeader_ = mk . BC.pack <$> maybestringopt "capabilities-header" rawopts
      , cliopts_ = cliopts
      }
  where
    stripTrailingSlash = reverse . dropWhile (== '/') . reverse -- yesod don't like it

checkWebOpts :: WebOpts -> WebOpts
checkWebOpts wopts = do
  let h = host_ wopts
  if any (`notElem` (".0123456789" :: String)) h
    then usageError $ "--host requires an IP address, not " ++ show h
    else wopts

getHledgerWebOpts :: IO WebOpts
getHledgerWebOpts = do
  args <- fmap replaceNumericFlags . expandArgsAt =<< getArgs
  rawOptsToWebOpts . decodeRawOpts . either usageError id $ process webmode args

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
