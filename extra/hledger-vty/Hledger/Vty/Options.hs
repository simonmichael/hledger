{-# LANGUAGE CPP #-}
{-|

-}

module Hledger.Vty.Options
where
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit

import Hledger.Cli hiding (progname,version,prognameandversion)

progname, version :: String
progname = "hledger-vty"
#ifdef VERSION
version = VERSION
#else
version = ""
#endif
prognameandversion :: String
prognameandversion = progname ++ " " ++ version :: String

vtyflags = [
  flagNone ["debug-vty"]  (\opts -> setboolopt "rules-file" opts) "run with no terminal output, showing console"
 ]

--vtymode :: Mode [([Char], [Char])]
vtymode =  (mode "hledger-vty" [("command","vty")]
            "browse accounts, postings and entries in a full-window curses interface"
            (argsFlag "[PATTERNS]") []){
              modeGroupFlags = Group {
                                groupUnnamed = vtyflags
                               ,groupHidden = []
                               ,groupNamed = [(generalflagsgroup1)]
                               }
             ,modeHelpSuffix=[
                  -- "Reads your ~/.hledger.journal file, or another specified by $LEDGER_FILE or -f, and starts the full-window curses ui."
                 ]
           }

-- hledger-vty options, used in hledger-vty and above
data VtyOpts = VtyOpts {
     debug_vty_ :: Bool
    ,cliopts_   :: CliOpts
 } deriving (Show)

defvtyopts = VtyOpts
    def
    def

-- instance Default CliOpts where def = defcliopts

toVtyOpts :: RawOpts -> IO VtyOpts
toVtyOpts rawopts = do
  cliopts <- rawOptsToCliOpts rawopts
  return defvtyopts {
              debug_vty_ = boolopt "debug-vty" rawopts
             ,cliopts_   = cliopts
             }

checkVtyOpts :: VtyOpts -> IO VtyOpts
checkVtyOpts opts = do
  checkCliOpts $ cliopts_ opts
  return opts

getHledgerVtyOpts :: IO VtyOpts
getHledgerVtyOpts = processArgs vtymode >>= return . decodeRawOpts >>= toVtyOpts >>= checkVtyOpts

