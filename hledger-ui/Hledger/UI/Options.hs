{-# LANGUAGE CPP #-}
{-|

-}

module Hledger.UI.Options
where
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit

import Hledger.Cli hiding (progname,version,prognameandversion)

progname, version :: String
progname = "hledger-ui"
#ifdef VERSION
version = VERSION
#else
version = ""
#endif
prognameandversion :: String
prognameandversion = progname ++ " " ++ version :: String

uiflags = [
  flagNone ["debug-ui"]  (\opts -> setboolopt "rules-file" opts) "run with no terminal output, showing console"
  ,flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show full account names, unindented"
  ,flagReq ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "with --flat, omit this many leading account name components"
  ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "use this custom line format"
  ,flagNone ["no-elide"] (\opts -> setboolopt "no-elide" opts) "no eliding at all, stronger than --empty"
  -- ,flagNone ["no-total"] (\opts -> setboolopt "no-total" opts) "don't show the final total"
 ]

--uimode :: Mode [([Char], [Char])]
uimode =  (mode "hledger-ui" [("command","ui")]
            "browse accounts, postings and entries in a full-window curses interface"
            (argsFlag "[PATTERNS]") []){
              modeGroupFlags = Group {
                                groupUnnamed = uiflags
                               ,groupHidden = []
                               ,groupNamed = [(generalflagsgroup1)]
                               }
             ,modeHelpSuffix=[
                  -- "Reads your ~/.hledger.journal file, or another specified by $LEDGER_FILE or -f, and starts the full-window curses ui."
                 ]
           }

-- hledger-ui options, used in hledger-ui and above
data UIOpts = UIOpts {
     debug_ui_ :: Bool
    ,cliopts_   :: CliOpts
 } deriving (Show)

defuiopts = UIOpts
    def
    def

-- instance Default CliOpts where def = defcliopts

toUIOpts :: RawOpts -> IO UIOpts
toUIOpts rawopts = do
  cliopts <- rawOptsToCliOpts rawopts
  return defuiopts {
              debug_ui_ = boolopt "debug-ui" rawopts
             ,cliopts_   = cliopts
             }

checkUIOpts :: UIOpts -> IO UIOpts
checkUIOpts opts = do
  checkCliOpts $ cliopts_ opts
  return opts

getHledgerUIOpts :: IO UIOpts
getHledgerUIOpts = processArgs uimode >>= return . decodeRawOpts >>= toUIOpts >>= checkUIOpts

