{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DeriveDataTypeable #-}
{-|

Common command-line options and utilities used by hledger's subcommands and addons.

-}

module Hledger.Cli.Options (

  -- * cmdargs modes & flags
  -- | These tell cmdargs how to parse the command line arguments for each hledger subcommand.
  argsFlag,
  defAddonCommandMode,
  defCommandMode,
  defMode,
  generalflagsgroup1,
  generalflagsgroup2,
  generalflagsgroup3,
  helpflags,
  inputflags,
  reportflags,
  
  -- * Raw options
  -- | To allow the cmdargs modes to be reused and extended by other
  -- packages (eg, add-ons which want to mimic the standard hledger
  -- options), our cmdargs modes parse to an extensible association
  -- list (RawOpts) rather than a closed ADT like CliOpts.
  RawOpts,
  boolopt,
  inRawOpts,
  intopt,
  listofstringopt,
  maybeintopt,
  maybestringopt,
  setboolopt,
  setopt,
  stringopt,

  -- * CLI options
  -- | Raw options are converted to a more convenient,
  -- package-specific options structure. This is the \"opts\" used
  -- throughout hledger CLI code.
  CliOpts(..),
  defcliopts,

  -- * CLI option accessors
  -- | Some options require more processing. Possibly these should be merged into argsToCliOpts.
  OutputWidth(..),
  Width(..),
  aliasesFromOpts,
  defaultWidth,
  defaultWidthWithFlag,
  formatFromOpts,
  journalFilePathFromOpts,
  rulesFilePathFromOpts,
  widthFromOpts,

  -- * utilities
  checkCliOpts,
  debugArgs,
  decodeRawOpts,
  getCliOpts,
  getHledgerAddonCommands,
  optserror,
  rawOptsToCliOpts,
  showModeHelp,
  withAliases,

  -- * tests
  tests_Hledger_Cli_Options

) 
where
  
import qualified Control.Exception as C
-- import Control.Monad (filterM)
import Control.Monad (when)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Time.Calendar
import Safe
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
import System.Directory
import System.Environment
import System.Exit
import Test.HUnit
import Text.ParserCombinators.Parsec as P

import Hledger
import Hledger.Data.OutputFormat as OutputFormat
import Hledger.Cli.Version

-- 
-- 1. cmdargs mode and flag (option) definitions for the hledger CLI,
-- can be reused by other packages as well.
--

-- | Our cmdargs modes parse arguments into an association list for better reuse.
type RawOpts = [(String,String)]

-- common flags and flag groups

-- | Common help flags: --help, --debug, --version...
helpflags = [
  flagNone ["help","h","?"] (setboolopt "help") "Display general help or (with --help after COMMAND) command help."
 -- ,flagNone ["browse-args"] (setboolopt "browse-args") "use a web UI to select options and build up a command line"
 ,flagOpt "1" ["debug"] (\s opts -> Right $ setopt "debug" s opts) "N" "Show debug output (optional argument sets debug level)"
 ,flagNone ["version"] (setboolopt "version") "Print version information"
 ]

-- | Common input-related flags: --file, --rules-file, --alias...
inputflags = [
  flagReq ["file","f"]  (\s opts -> Right $ setopt "file" s opts) "FILE" "use a different journal file; - means stdin"
 ,flagReq ["rules-file"]  (\s opts -> Right $ setopt "rules-file" s opts) "RULESFILE" "conversion rules for CSV (default: FILE.rules)"
 ,flagReq ["alias"]  (\s opts -> Right $ setopt "alias" s opts)  "ACCT=ALIAS" "convert ACCT's name to ALIAS"
 ]

-- | Common report-related flags: --period, --cost, --display etc.
reportflags = [
  flagReq  ["begin","b"]     (\s opts -> Right $ setopt "begin" s opts) "DATE" "report on transactions on or after this date"
 ,flagReq  ["end","e"]       (\s opts -> Right $ setopt "end" s opts) "DATE" "report on transactions before this date"
 ,flagReq  ["period","p"]    (\s opts -> Right $ setopt "period" s opts) "PERIODEXP" "report on transactions during the specified period and/or with the specified reporting interval"
 ,flagNone ["daily","D"]     (\opts -> setboolopt "daily" opts) "report by day"
 ,flagNone ["weekly","W"]    (\opts -> setboolopt "weekly" opts) "report by week"
 ,flagNone ["monthly","M"]   (\opts -> setboolopt "monthly" opts) "report by month"
 ,flagNone ["quarterly","Q"] (\opts -> setboolopt "quarterly" opts) "report by quarter"
 ,flagNone ["yearly","Y"]    (\opts -> setboolopt "yearly" opts) "report by year"
 ,flagNone ["cleared","C"]   (\opts -> setboolopt "cleared" opts) "report only on cleared transactions"
 ,flagNone ["uncleared","U"] (\opts -> setboolopt "uncleared" opts) "report only on uncleared transactions"
 ,flagNone ["cost","B"]      (\opts -> setboolopt "cost" opts) "report cost of commodities"
 ,flagReq  ["depth"]         (\s opts -> Right $ setopt "depth" s opts) "N" "hide accounts/transactions deeper than this"
 ,flagReq  ["display","d"]   (\s opts -> Right $ setopt "display" s opts) "DISPLAYEXP" "show only transactions matching the expression, which is 'dOP[DATE]' where OP is <, <=, =, >=, >"
 ,flagNone ["date2","aux-date","effective"]     (\opts -> setboolopt "date2" opts) "use transactions' secondary dates, if any"
 ,flagNone ["empty","E"]     (\opts -> setboolopt "empty" opts) "show empty/zero things which are normally elided"
 ,flagNone ["real","R"]      (\opts -> setboolopt "real" opts) "report only on real (non-virtual) transactions"
 ]

argsFlag desc = flagArg (\s opts -> Right $ setopt "args" s opts) desc

generalflagstitle = "\nGeneral flags"
generalflagsgroup1 = (generalflagstitle, inputflags ++ reportflags ++ helpflags)
generalflagsgroup2 = (generalflagstitle, inputflags ++ helpflags)
generalflagsgroup3 = (generalflagstitle, helpflags)

-- cmdargs mode constructors

-- | A basic mode template.
defMode :: Mode RawOpts
defMode =   Mode {
  modeNames = []
 ,modeHelp = ""
 ,modeHelpSuffix = []
 ,modeValue = []
 ,modeCheck = Right
 ,modeReform = const Nothing
 ,modeExpandAt = True
 ,modeGroupFlags = Group {
     groupNamed = []
    ,groupUnnamed = [
        flagNone ["help","h","?"] (setboolopt "help") "Display command help."
        ]
    ,groupHidden = []
    }
 ,modeArgs = ([], Nothing)
 ,modeGroupModes = toGroup []
 }

-- | A basic subcommand mode with the given command name(s).
defCommandMode names = defMode {
   modeNames=names
  ,modeValue=[("command", headDef "" names)]
  ,modeArgs = ([], Just $ argsFlag "[PATTERNS]")
  }

-- | A basic subcommand mode suitable for an add-on command.
defAddonCommandMode addon = defMode {
   modeNames = [addon]
  ,modeHelp = fromMaybe "" $ lookup (striphs addon) standardAddonsHelp
  ,modeValue=[("command",addon)]
  ,modeGroupFlags = Group {
      groupUnnamed = []
     ,groupHidden = []
     ,groupNamed = [generalflagsgroup1]
     }
  ,modeArgs = ([], Just $ argsFlag "[ARGS]")
  }

striphs = regexReplace "\\.l?hs$" ""

-- | Built-in descriptions for some of the known external addons,
-- since we don't currently have any way to ask them.
standardAddonsHelp :: [(String,String)]
standardAddonsHelp = [
   ("chart", "generate simple balance pie charts")
  ,("interest", "generate interest transaction entries")
  ,("irr", "calculate internal rate of return")
  ,("vty", "start the curses-style interface")
  ,("web", "start the web interface")
  ,("accounts", "list account names")
  ,("balance-csv", "output a balance report as CSV")
  ,("equity", "show a transaction entry zeroing all accounts")
  ,("print-unique", "print only transactions with unique descriptions")
  ,("register-csv", "output a register report as CSV")
  ,("rewrite", "add specified postings to matched transaction entries")
  ]

-- | Add command aliases to the command's help string.
withAliases :: String -> [String] -> String
s `withAliases` as = s ++ " (" ++ intercalate ", " as ++ ")"
-- s `withAliases` []     = s
-- s `withAliases` (a:[]) = s ++ " (alias: " ++ a ++ ")"
-- s `withAliases` as     = s ++ " (aliases: " ++ intercalate ", " as ++ ")"


-- help_postscript = [
--   -- "DATES can be Y/M/D or smart dates like \"last month\"."
--   -- ,"PATTERNS are regular"
--   -- ,"expressions which filter by account name.  Prefix a pattern with desc: to"
--   -- ,"filter by transaction description instead, prefix with not: to negate it."
--   -- ,"When using both, not: comes last."
--  ]

--
-- 2. A package-specific data structure holding options used in this
-- package and above, parsed from RawOpts.  This represents the
-- command-line options that were provided, with all parsing
-- completed, but before adding defaults or derived values (XXX add)
--

-- | Command line options. Used in the @hledger@ package and above.
data CliOpts = CliOpts {
     rawopts_         :: RawOpts
    ,command_         :: String
    ,file_            :: Maybe FilePath
    ,rules_file_      :: Maybe FilePath
    ,alias_           :: [String]
    ,debug_           :: Int            -- ^ debug level, set by @--debug[=N]@. See also 'Hledger.Utils.debugLevel'.
    ,no_new_accounts_ :: Bool           -- add
    ,width_           :: Maybe String   -- register
    ,reportopts_      :: ReportOpts
 } deriving (Show, Data, Typeable)

defcliopts = CliOpts
    def
    def
    def
    def
    def
    def
    def
    def
    def

instance Default CliOpts where def = defcliopts

-- | Parse raw option string values to the desired final data types.
-- Any relative smart dates will be converted to fixed dates based on
-- today's date. Parsing failures will raise an error.
rawOptsToCliOpts :: RawOpts -> IO CliOpts
rawOptsToCliOpts rawopts = do
  d <- getCurrentDay
  return defcliopts {
              rawopts_         = rawopts
             ,command_         = stringopt "command" rawopts
             ,file_            = maybestringopt "file" rawopts
             ,rules_file_      = maybestringopt "rules-file" rawopts
             ,alias_           = map stripquotes $ listofstringopt "alias" rawopts
             ,debug_           = intopt "debug" rawopts
             ,no_new_accounts_ = boolopt "no-new-accounts" rawopts -- add
             ,width_           = maybestringopt "width" rawopts    -- register
             ,reportopts_ = defreportopts {
                             begin_     = maybesmartdateopt d "begin" rawopts
                            ,end_       = maybesmartdateopt d "end" rawopts
                            ,period_    = maybeperiodopt d rawopts
                            ,cleared_   = boolopt "cleared" rawopts
                            ,uncleared_ = boolopt "uncleared" rawopts
                            ,cost_      = boolopt "cost" rawopts
                            ,depth_     = maybeintopt "depth" rawopts
                            ,display_   = maybedisplayopt d rawopts
                            ,date2_     = boolopt "date2" rawopts
                            ,empty_     = boolopt "empty" rawopts
                            ,no_elide_  = boolopt "no-elide" rawopts
                            ,real_      = boolopt "real" rawopts
                            ,balancetype_ = balancetypeopt rawopts -- balance
                            ,flat_      = boolopt "flat" rawopts -- balance
                            ,drop_      = intopt "drop" rawopts -- balance
                            ,no_total_  = boolopt "no-total" rawopts -- balance
                            ,daily_     = boolopt "daily" rawopts
                            ,weekly_    = boolopt "weekly" rawopts
                            ,monthly_   = boolopt "monthly" rawopts
                            ,quarterly_ = boolopt "quarterly" rawopts
                            ,yearly_    = boolopt "yearly" rawopts
                            ,format_    = maybestringopt "format" rawopts
                            ,average_   = boolopt "average" rawopts  -- register
                            ,related_   = boolopt "related" rawopts  -- register
                            ,query_     = unwords $ listofstringopt "args" rawopts -- doesn't handle an arg like "" right
                            }
             }

-- | Convert possibly encoded option values to regular unicode strings.
decodeRawOpts = map (\(name,val) -> (name, fromSystemString val))

-- | Do final validation of processed opts, raising an error if there is trouble.
checkCliOpts :: CliOpts -> IO CliOpts -- or pure..
checkCliOpts opts@CliOpts{reportopts_=ropts} = do
  case formatFromOpts ropts of
    Left err -> optserror $ "could not parse format option: "++err
    Right _ -> return ()
  case widthFromOpts opts of
    Left err -> optserror $ "could not parse width option: "++err
    Right _ -> return ()
  return opts

--
-- utils
--

-- | Get the unique suffixes (without hledger-) of hledger-* executables
-- found in the current user's PATH, or the empty list if there is any
-- problem.
getHledgerAddonCommands :: IO [String]
getHledgerAddonCommands = map (drop (length progname + 1)) `fmap` getHledgerExesInPath

-- | Get the unique names of hledger-*{,.hs} executables found in the current
-- user's PATH, or the empty list if there is any problem.
getHledgerExesInPath :: IO [String]
getHledgerExesInPath = do
  pathdirs <- splitOn ":" `fmap` getEnvSafe "PATH"
  pathfiles <- concat `fmap` mapM getDirectoryContentsSafe pathdirs
  let hledgernamed = nubBy (\a b -> striphs a == striphs b) $ sort $ filter isHledgerExeName pathfiles
                       where striphs = regexReplace "\\.l?hs$" ""
  -- hledgerexes <- filterM isExecutable hledgernamed
  return hledgernamed

-- isExecutable f = getPermissions f >>= (return . executable)

isHledgerExeName = isRight . parsewith hledgerexenamep
    where
      hledgerexenamep = do
        string progname
        char '-'
        many1 (noneOf ".")
        optional (string ".hs" <|> string ".lhs")
        eof

getEnvSafe v = getEnv v `C.catch` (\(_::C.IOException) -> return "")

getDirectoryContentsSafe d =
    (filter (not . (`elem` [".",".."])) `fmap` getDirectoryContents d) `C.catch` (\(_::C.IOException) -> return [])

-- | Raise an error, showing the specified message plus a hint about --help.
optserror = error' . (++ " (run with --help for usage)")

setopt name val = (++ [(name,singleQuoteIfNeeded val)])

setboolopt name = (++ [(name,"")])

-- | Is the named option present ?
inRawOpts :: String -> RawOpts -> Bool
inRawOpts name = isJust . lookup name

boolopt = inRawOpts

maybestringopt name = maybe Nothing (Just . stripquotes) . lookup name

stringopt name = fromMaybe "" . maybestringopt name

listofstringopt name rawopts = [v | (k,v) <- rawopts, k==name]

maybeintopt :: String -> RawOpts -> Maybe Int
maybeintopt name rawopts =
    let ms = maybestringopt name rawopts in
    case ms of Nothing -> Nothing
               Just s -> Just $ readDef (optserror $ "could not parse "++name++" number: "++s) s

intopt name = fromMaybe 0 . maybeintopt name

maybesmartdateopt :: Day -> String -> RawOpts -> Maybe Day
maybesmartdateopt d name rawopts =
        case maybestringopt name rawopts of
          Nothing -> Nothing
          Just s -> either
                    (\e -> optserror $ "could not parse "++name++" date: "++show e)
                    Just
                    $ fixSmartDateStrEither' d s

maybedisplayopt :: Day -> RawOpts -> Maybe DisplayExp
maybedisplayopt d rawopts =
    maybe Nothing (Just . regexReplaceBy "\\[.+?\\]" fixbracketeddatestr) $ maybestringopt "display" rawopts
    where
      fixbracketeddatestr "" = ""
      fixbracketeddatestr s = "[" ++ fixSmartDateStr d (init $ tail s) ++ "]"

maybeperiodopt :: Day -> RawOpts -> Maybe (Interval,DateSpan)
maybeperiodopt d rawopts =
    case maybestringopt "period" rawopts of
      Nothing -> Nothing
      Just s -> either
                (\e -> optserror $ "could not parse period option: "++show e)
                Just
                $ parsePeriodExpr d s

balancetypeopt :: RawOpts -> BalanceType
balancetypeopt rawopts
    | length [o | o <- ["cumulative","historical"], isset o] > 1
                         = optserror "please specify at most one of --cumulative and --historical"
    | isset "cumulative" = CumulativeBalance
    | isset "historical" = HistoricalBalance
    | otherwise          = PeriodBalance
    where
      isset = flip boolopt rawopts

-- | Parse the format option if provided, possibly returning an error,
-- otherwise get the default value.
formatFromOpts :: ReportOpts -> Either String [OutputFormat]
formatFromOpts = maybe (Right defaultBalanceFormat) parseStringFormat . format_

-- | Default line format for balance report: "%20(total)  %2(depth_spacer)%-(account)"
defaultBalanceFormat :: [OutputFormat]
defaultBalanceFormat = [
      FormatField False (Just 20) Nothing TotalField
    , FormatLiteral "  "
    , FormatField True (Just 2) Nothing DepthSpacerField
    , FormatField True Nothing Nothing AccountField
    ]

-- | Output width configuration (for register).
data OutputWidth =
    TotalWidth Width    -- ^ specify the overall width 
  | FieldWidths [Width] -- ^ specify each field's width
  deriving Show

-- | A width value.
data Width =
    Width Int -- ^ set width to exactly this number of characters
  | Auto      -- ^ set width automatically from available space
  deriving Show

-- | Default width of hledger console output.
defaultWidth         = 80

-- | Width of hledger console output when the -w flag is used with no value.
defaultWidthWithFlag = 120

-- | Parse the width option if provided, possibly returning an error,
-- otherwise get the default value.
widthFromOpts :: CliOpts -> Either String OutputWidth
widthFromOpts CliOpts{width_=Nothing} = Right $ TotalWidth $ Width defaultWidth
widthFromOpts CliOpts{width_=Just ""} = Right $ TotalWidth $ Width defaultWidthWithFlag
widthFromOpts CliOpts{width_=Just s}  = parseWidth s

parseWidth :: String -> Either String OutputWidth
parseWidth s = case (runParser outputwidthp () "(unknown)") s of
    Left  e -> Left $ show e
    Right x -> Right x

outputwidthp :: GenParser Char st OutputWidth
outputwidthp =
  try (do w <- widthp
          ws <- many1 (char ',' >> widthp)
          return $ FieldWidths $ w:ws)
  <|> TotalWidth `fmap` widthp

widthp :: GenParser Char st Width
widthp = (string "auto" >> return Auto)
    <|> (Width . read) `fmap` many1 digit

-- | Get the account name aliases from options, if any.
aliasesFromOpts :: CliOpts -> [(AccountName,AccountName)]
aliasesFromOpts = map parseAlias . alias_
    where
      -- similar to ledgerAlias
      parseAlias :: String -> (AccountName,AccountName)
      parseAlias s = (accountNameWithoutPostingType $ strip orig
                     ,accountNameWithoutPostingType $ strip alias')
          where
            (orig, alias) = break (=='=') s
            alias' = case alias of ('=':rest) -> rest
                                   _ -> orig

-- | Get the (tilde-expanded, absolute) journal file path from
-- 1. options, 2. an environment variable, or 3. the default.
journalFilePathFromOpts :: CliOpts -> IO String
journalFilePathFromOpts opts = do
  f <- defaultJournalPath
  d <- getCurrentDirectory
  expandPath d $ fromMaybe f $ file_ opts

-- | Get the (tilde-expanded) rules file path from options, if any.
rulesFilePathFromOpts :: CliOpts -> IO (Maybe FilePath)
rulesFilePathFromOpts opts = do
  d <- getCurrentDirectory
  maybe (return Nothing) (fmap Just . expandPath d) $ rules_file_ opts

-- | Get a mode's help message as a nicely wrapped string.
showModeHelp :: Mode a -> String
showModeHelp =
  (showText defaultWrap :: [Text] -> String) . 
  (helpText [] HelpFormatDefault :: Mode a -> [Text])

-- not used:

-- | Print debug info about arguments and options if --debug is present.
debugArgs :: [String] -> CliOpts -> IO ()
debugArgs args opts =
  when ("--debug" `elem` args) $ do
    progname <- getProgName
    putStrLn $ "running: " ++ progname
    putStrLn $ "raw args: " ++ show args
    putStrLn $ "processed opts:\n" ++ show opts
    d <- getCurrentDay
    putStrLn $ "search query: " ++ (show $ queryFromOpts d $ reportopts_ opts)

-- not used:

-- | Parse hledger CLI options from the command line using the given
-- cmdargs mode, and either return them or, if a help flag is present,
-- print the mode help and exit the program.
getCliOpts :: Mode RawOpts -> IO CliOpts
getCliOpts mode = do
  args <- getArgs
  let rawopts = decodeRawOpts $ processValue mode args
  opts <- rawOptsToCliOpts rawopts >>= checkCliOpts
  debugArgs args opts
  -- if any (`elem` args) ["--help","-h","-?"]
  when ("help" `inRawOpts` rawopts_ opts) $
    putStr (showModeHelp mode) >> exitSuccess
  return opts

tests_Hledger_Cli_Options = TestList
 [
 ]
