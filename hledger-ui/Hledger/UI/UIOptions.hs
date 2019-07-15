{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|

-}

module Hledger.UI.UIOptions
where
import Data.Data (Data)
import Data.Default
import Data.Typeable (Typeable)
import Data.List (intercalate)
import System.Environment

import Hledger.Cli hiding (progname,version,prognameandversion)
import Hledger.UI.Theme (themeNames)

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
  ,flagNone ["flat","F"] (setboolopt "flat") "show accounts as a list (default)"
  ,flagNone ["tree","T"] (setboolopt "tree") "show accounts as a tree"
--  ,flagNone ["present"] (setboolopt "present") "exclude transactions dated later than today (default)"
  ,flagNone ["future"] (setboolopt "future") "show transactions dated later than today (normally hidden)"
  -- ,flagReq ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "with --flat, omit this many leading account name components"
  -- ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "use this custom line format"
  -- ,flagNone ["no-elide"] (setboolopt "no-elide") "don't compress empty parent accounts on one line"
 ]

--uimode :: Mode [([Char], [Char])]
uimode =  (mode "hledger-ui" [("command","ui")]
            "browse accounts, postings and entries in a full-window curses interface"
            (argsFlag "[PATTERNS]") []){
              modeGroupFlags = Group {
                                groupUnnamed = uiflags
                               ,groupHidden = hiddenflags
                               ,groupNamed = [(generalflagsgroup1)]
                               }
             ,modeHelpSuffix=[
                  -- "Reads your ~/.hledger.journal file, or another specified by $LEDGER_FILE or -f, and starts the full-window curses ui."
                 ]
           }

-- hledger-ui options, used in hledger-ui and above
data UIOpts = UIOpts {
     watch_   :: Bool
    ,change_  :: Bool
    ,presentorfuture_  :: PresentOrFutureOpt
    ,cliopts_ :: CliOpts
 } deriving (Show)

defuiopts = UIOpts
    def
    def
    def
    def

-- instance Default CliOpts where def = defcliopts

rawOptsToUIOpts :: RawOpts -> IO UIOpts
rawOptsToUIOpts rawopts = checkUIOpts <$> do
  cliopts <- rawOptsToCliOpts rawopts
  return defuiopts {
              watch_   = boolopt "watch" rawopts
             ,change_  = boolopt "change" rawopts
             ,presentorfuture_ = presentorfutureopt rawopts
             ,cliopts_ = cliopts
             }

-- | Should transactions dated later than today be included ?
-- Like flat/tree mode, there are three states, and the meaning of default can vary by command.
data PresentOrFutureOpt = PFDefault | PFPresent | PFFuture deriving (Eq, Show, Data, Typeable)
instance Default PresentOrFutureOpt where def = PFDefault

presentorfutureopt :: RawOpts -> PresentOrFutureOpt
presentorfutureopt rawopts =
  case reverse $ filter (`elem` ["present","future"]) $ map fst rawopts of
    ("present":_) -> PFPresent
    ("future":_)  -> PFFuture
    _             -> PFDefault

checkUIOpts :: UIOpts -> UIOpts
checkUIOpts opts =
  either usageError (const opts) $ do
    case maybestringopt "theme" $ rawopts_ $ cliopts_ opts of
      Just t | not $ elem t themeNames -> Left $ "invalid theme name: "++t
      _                                -> Right ()

-- XXX some refactoring seems due
getHledgerUIOpts :: IO UIOpts
--getHledgerUIOpts = processArgs uimode >>= return . decodeRawOpts >>= rawOptsToUIOpts
getHledgerUIOpts = do
  args <- getArgs >>= expandArgsAt
  let args' = replaceNumericFlags args
  let cmdargopts = either usageError id $ process uimode args'
  rawOptsToUIOpts $ decodeRawOpts cmdargopts

