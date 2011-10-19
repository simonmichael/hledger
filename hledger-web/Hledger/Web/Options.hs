{-# LANGUAGE TemplateHaskell #-}
{-|

-}

module Hledger.Web.Options
where
import Data.Maybe
import Distribution.PackageDescription.TH (packageVariable, package, pkgName, pkgVersion)
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit

import Hledger.Cli hiding (progname,progversion)
import Hledger.Web.Settings

progname    = $(packageVariable (pkgName . package))
progversion = progname ++ " " ++ $(packageVariable (pkgVersion . package)) :: String

defbaseurlexample = (reverse $ drop 4 $ reverse $ defbaseurl defport) ++ "PORT"

webflags = [
  flagReq ["base-url"]  (\s opts -> Right $ setopt "base-url" s opts) "URL" ("set the base url (default: "++defbaseurlexample++")")
 ,flagReq ["port"]  (\s opts -> Right $ setopt "port" s opts) "PORT" ("listen on this tcp port (default: "++show defport++")")
 ]
 
webmode =  (mode "hledger-web" [("command","web")]
            "start serving the hledger web interface"
            mainargsflag []){
              modeGroupFlags = Group {
                                groupUnnamed = webflags
                               ,groupHidden = []
                               ,groupNamed = [(generalflagstitle, generalflags1)]
                               }
             ,modeHelpSuffix=[
                  -- "Reads your ~/.hledger.journal file, or another specified by $LEDGER_FILE or -f, and starts the full-window curses ui."
                 ]
           }

-- hledger-web options, used in hledger-web and above
data WebOpts = WebOpts {
     base_url_ :: String
    ,port_     :: Int
    ,cliopts_  :: CliOpts
 } deriving (Show)

defwebopts = WebOpts
    def
    def
    def

-- instance Default WebOpts where def = defwebopts

toWebOpts :: RawOpts -> IO WebOpts
toWebOpts rawopts = do
  cliopts <- toCliOpts rawopts
  let p = fromMaybe defport $ maybeintopt "port" rawopts
  return defwebopts {
              port_ = p
             ,base_url_ = maybe (defbaseurl p) stripTrailingSlash $ maybestringopt "base-url" rawopts
             ,cliopts_   = cliopts
             }
  where
    stripTrailingSlash = reverse . dropWhile (=='/') . reverse -- yesod don't like it

checkWebOpts :: WebOpts -> IO WebOpts
checkWebOpts opts = do
  checkCliOpts $ cliopts_ opts
  return opts

getHledgerWebOpts :: IO WebOpts
getHledgerWebOpts = processArgs webmode >>= return . decodeRawOpts >>= toWebOpts >>= checkWebOpts

