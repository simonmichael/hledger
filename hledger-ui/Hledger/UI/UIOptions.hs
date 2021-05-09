{-# LANGUAGE CPP #-}
{-|

-}

module Hledger.UI.UIOptions
where
import Data.Default
import Data.List (intercalate)
import System.Environment

import Hledger.Cli hiding (progname,version,prognameandversion)
import Hledger.UI.Theme (themeNames)

progname, version, prognameandversion :: String
progname = "hledger-ui"
#ifdef VERSION
version = VERSION
#else
version = ""
#endif
prognameandversion = versiondescription progname

uiflags = [
  -- flagNone ["debug-ui"] (setboolopt "rules-file") "run with no terminal output, showing console"
   flagNone ["watch"] (setboolopt "watch") "watch for data and date changes and reload automatically"
  ,flagReq  ["theme"] (\s opts -> Right $ setopt "theme" s opts) "THEME" ("use this custom display theme ("++intercalate ", " themeNames++")")
  ,flagReq  ["register"] (\s opts -> Right $ setopt "register" s opts) "ACCTREGEX" "start in the (first) matched account's register"
  ,flagNone ["change"] (setboolopt "change")
    "show period balances (changes) at startup instead of historical balances"
  -- ,flagNone ["cumulative"] (setboolopt "cumulative")
  --   "show balance change accumulated across periods (in multicolumn reports)"
  -- ,flagNone ["historical","H"] (setboolopt "historical")
  --   "show historical ending balance in each period (includes postings before report start date)\n "
  ]
  ++ flattreeflags False
--  ,flagNone ["present"] (setboolopt "present") "exclude transactions dated later than today (default)"
  -- ,flagReq ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "with --flat, omit this many leading account name components"
  -- ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "use this custom line format"
  -- ,flagNone ["no-elide"] (setboolopt "no-elide") "don't compress empty parent accounts on one line"

--uimode :: Mode RawOpts
uimode =  (mode "hledger-ui" (setopt "command" "ui" def)
            "browse accounts, postings and entries in a full-window curses interface"
            (argsFlag "[PATTERNS]") []){
              modeGroupFlags = Group {
                                groupUnnamed = uiflags
                               ,groupHidden = hiddenflags
                                 ++ [flagNone ["future"] (setboolopt "forecast") "compatibility alias, use --forecast instead"]
                               ,groupNamed = [(generalflagsgroup1)]
                               }
             ,modeHelpSuffix=[
                  -- "Reads your ~/.hledger.journal file, or another specified by $LEDGER_FILE or -f, and starts the full-window curses ui."
                 ]
           }

-- hledger-ui options, used in hledger-ui and above
data UIOpts = UIOpts {
     watch_       :: Bool
    ,change_      :: Bool
    ,cliopts_     :: CliOpts
 } deriving (Show)

defuiopts = UIOpts
  { watch_   = False
  , change_  = False
  , cliopts_ = def
  }

-- instance Default CliOpts where def = defcliopts

rawOptsToUIOpts :: RawOpts -> IO UIOpts
rawOptsToUIOpts rawopts = checkUIOpts <$> do
  cliopts <- rawOptsToCliOpts rawopts
  return defuiopts {
              watch_       = boolopt "watch" rawopts
             ,change_      = boolopt "change" rawopts
             ,cliopts_     = cliopts
             }

checkUIOpts :: UIOpts -> UIOpts
checkUIOpts opts =
  either usageError (const opts) $ do
    case maybestringopt "theme" $ rawopts_ $ cliopts_ opts of
      Just t | not $ elem t themeNames -> Left $ "invalid theme name: "++t
      _                                -> Right ()

-- XXX some refactoring seems due
getHledgerUIOpts :: IO UIOpts
--getHledgerUIOpts = processArgs uimode >>= return >>= rawOptsToUIOpts
getHledgerUIOpts = do
  args <- getArgs >>= expandArgsAt
  let args' = replaceNumericFlags args
  let cmdargopts = either usageError id $ process uimode args'
  rawOptsToUIOpts cmdargopts
