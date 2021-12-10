{-# LANGUAGE CPP #-}
{-|

-}

module Hledger.UI.UIOptions
where

import Data.Default (def)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Lens.Micro (set)
import System.Environment (getArgs)

import Hledger.Cli hiding (packageversion, progname, prognameandversion)
import Hledger.UI.Theme (themes, themeNames)

-- cf Hledger.Cli.Version

packageversion :: PackageVersion
packageversion =
#ifdef VERSION
  VERSION
#else
  ""
#endif

progname :: ProgramName
progname = "hledger-ui"

prognameandversion :: VersionString
prognameandversion = versionString progname packageversion

uiflags = [
  -- flagNone ["debug-ui"] (setboolopt "rules-file") "run with no terminal output, showing console"
   flagNone ["watch","w"] (setboolopt "watch") "watch for data and date changes and reload automatically"
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
data UIOpts = UIOpts
  { uoWatch    :: Bool
  , uoTheme    :: Maybe String
  , uoRegister :: Maybe String
  , uoCliOpts  :: CliOpts
  } deriving (Show)

defuiopts = UIOpts
  { uoWatch    = False
  , uoTheme    = Nothing
  , uoRegister = Nothing
  , uoCliOpts  = defcliopts
  }

-- | Process a RawOpts into a UIOpts.
-- This will return a usage error if provided an invalid theme.
rawOptsToUIOpts :: RawOpts -> IO UIOpts
rawOptsToUIOpts rawopts = do
    cliopts <- set balanceaccum accum <$> rawOptsToCliOpts rawopts
    return defuiopts {
                uoWatch    = boolopt "watch" rawopts
               ,uoTheme    = checkTheme <$> maybestringopt "theme" rawopts
               ,uoRegister = maybestringopt "register" rawopts
               ,uoCliOpts  = cliopts
               }
  where
    -- show historical balance by default (unlike hledger)
    accum = fromMaybe Historical $ balanceAccumulationOverride rawopts
    checkTheme t = if t `M.member` themes then t else usageError $ "invalid theme name: " ++ t

-- XXX some refactoring seems due
getHledgerUIOpts :: IO UIOpts
--getHledgerUIOpts = processArgs uimode >>= return >>= rawOptsToUIOpts
getHledgerUIOpts = do
  args <- getArgs >>= expandArgsAt
  let args' = replaceNumericFlags args
  let cmdargopts = either usageError id $ process uimode args'
  rawOptsToUIOpts cmdargopts

instance HasCliOpts UIOpts where
    cliOpts f uiopts = (\x -> uiopts{uoCliOpts=x}) <$> f (uoCliOpts uiopts)

instance HasInputOpts UIOpts where
    inputOpts = cliOpts.inputOpts

instance HasBalancingOpts UIOpts where
    balancingOpts = cliOpts.balancingOpts

instance HasReportSpec UIOpts where
    reportSpec = cliOpts.reportSpec

instance HasReportOptsNoUpdate UIOpts where
    reportOptsNoUpdate = cliOpts.reportOptsNoUpdate

instance HasReportOpts UIOpts where
    reportOpts = cliOpts.reportOpts
