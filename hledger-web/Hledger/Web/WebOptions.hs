{-# LANGUAGE CPP #-}
module Hledger.Web.WebOptions
where
import Prelude
import Data.Default
#if !MIN_VERSION_base(4,8,0)
import Data.Functor.Compat ((<$>))
#endif
import Data.Maybe
import System.Environment

import Hledger.Cli hiding (progname,version,prognameandversion)
import Settings

progname, version :: String
progname = "hledger-web"
#ifdef VERSION
version = VERSION
#else
version = ""
#endif
prognameandversion :: String
prognameandversion = progname ++ " " ++ version :: String

webflags :: [Flag [([Char], [Char])]]
webflags = [
  flagNone ["serve","server"]   (setboolopt "serve") ("serve and log requests, don't browse or auto-exit")
 ,flagReq  ["host"]     (\s opts -> Right $ setopt "host" s opts) "IPADDR" ("listen on this IP address (default: "++defhost++")")
 ,flagReq  ["port"]     (\s opts -> Right $ setopt "port" s opts) "PORT" ("listen on this TCP port (default: "++show defport++")")
 ,flagReq  ["base-url"] (\s opts -> Right $ setopt "base-url" s opts) "BASEURL" ("set the base url (default: http://IPADDR:PORT)")
 ,flagReq  ["file-url"] (\s opts -> Right $ setopt "file-url" s opts) "FILEURL" ("set the static files url (default: BASEURL/static)")
 ]

webmode :: Mode [([Char], [Char])]
webmode =  (mode "hledger-web" [("command","web")]
            "start serving the hledger web interface"
            (argsFlag "[PATTERNS]") []){
              modeGroupFlags = Group {
                                groupUnnamed = webflags
                               ,groupHidden = [flagNone ["binary-filename"] (setboolopt "binary-filename") "show the download filename for this executable, and exit"]
                               ,groupNamed = [generalflagsgroup1]
                               }
             ,modeHelpSuffix=[
                  -- "Reads your ~/.hledger.journal file, or another specified by $LEDGER_FILE or -f, and starts the full-window curses ui."
                 ]
           }

-- hledger-web options, used in hledger-web and above
data WebOpts = WebOpts {
     serve_    :: Bool
    ,host_     :: String
    ,port_     :: Int
    ,base_url_ :: String
    ,file_url_ :: Maybe String
    ,cliopts_  :: CliOpts
 } deriving (Show)

defwebopts :: WebOpts
defwebopts = WebOpts
    def
    def
    def
    def
    def
    def

-- instance Default WebOpts where def = defwebopts

rawOptsToWebOpts :: RawOpts -> IO WebOpts
rawOptsToWebOpts rawopts = checkWebOpts <$> do
  cliopts <- rawOptsToCliOpts rawopts
  let
    h = fromMaybe defhost $ maybestringopt "host" rawopts
    p = fromMaybe defport $ maybeintopt "port" rawopts
    b = maybe (defbaseurl h p) stripTrailingSlash $ maybestringopt "base-url" rawopts
  return defwebopts {
              serve_ = boolopt "serve" rawopts
             ,host_ = h
             ,port_ = p
             ,base_url_ = b
             ,file_url_ = stripTrailingSlash <$> maybestringopt "file-url" rawopts
             ,cliopts_   = cliopts
             }
  where
    stripTrailingSlash = reverse . dropWhile (=='/') . reverse -- yesod don't like it

checkWebOpts :: WebOpts -> WebOpts
checkWebOpts wopts =
  either usageError (const wopts) $ do
    let h = host_ wopts
    if any (not . (`elem` ".0123456789")) h
    then Left $ "--host requires an IP address, not "++show h
    else Right ()

getHledgerWebOpts :: IO WebOpts
--getHledgerWebOpts = processArgs webmode >>= return . decodeRawOpts >>= rawOptsToWebOpts
getHledgerWebOpts = do
  args <- getArgs >>= expandArgsAt
  let args' = replaceNumericFlags args 
  let cmdargopts = either usageError id $ process webmode args'
  rawOptsToWebOpts $ decodeRawOpts cmdargopts 

