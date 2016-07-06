{-|

Common cmdargs modes and flags, a command-line options type, and
related utilities used by hledger commands.

-}

{-# LANGUAGE CPP, ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts #-}

module Hledger.Cli.CliOptions (

  -- * cmdargs flags & modes
  helpflags,
  detailedversionflag,
  inputflags,
  reportflags,
  outputflags,
  generalflagsgroup1,
  generalflagsgroup2,
  generalflagsgroup3,
  defMode,
  defCommandMode,
  defAddonCommandMode,
  argsFlag,
  showModeUsage,
  withAliases,

  -- * CLI options
  CliOpts(..),
  defcliopts,
  getCliOpts,
  decodeRawOpts,
  rawOptsToCliOpts,
  checkCliOpts,
  outputFormats,
  defaultOutputFormat,
  defaultBalanceLineFormat,

  -- possibly these should move into argsToCliOpts
  -- * CLI option accessors
  -- | These do the extra processing required for some options.
  aliasesFromOpts,
  journalFilePathFromOpts,
  rulesFilePathFromOpts,
  outputFileFromOpts,
  outputFormatFromOpts,
  defaultWidth,
  widthFromOpts,
  -- | For register:
  registerWidthsFromOpts,
  maybeAccountNameDrop,
  -- | For balance:
  lineFormatFromOpts,

  -- * Other utils
  hledgerAddons,
  topicForMode,

  -- * Tests
  tests_Hledger_Cli_CliOptions

)
where

import Prelude ()
import Prelude.Compat
import qualified Control.Exception as C
import Control.Monad (when)
#if !MIN_VERSION_base(4,8,0)
import Data.Functor.Compat ((<$>))
#endif
import Data.List.Compat
import Data.List.Split (splitOneOf)
import Data.Maybe
-- import Data.Text (Text)
import qualified Data.Text as T
import Safe
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
#ifndef mingw32_HOST_OS
import System.Console.Terminfo
#endif
import System.Directory
import System.Environment
import System.Exit (exitSuccess)
import System.FilePath
import Test.HUnit
import Text.Parsec

import Hledger
import Hledger.Cli.DocFiles
import Hledger.Cli.Version


-- common cmdargs flags

-- | Common help flags: --help, --debug, --version...
helpflags :: [Flag RawOpts]
helpflags = [
  flagNone ["h"]    (setboolopt "h")    "show general usage or (after COMMAND, the command's usage"
 ,flagNone ["help"] (setboolopt "help") "show the current program's manual as plain text (or after an add-on COMMAND, the add-on's manual)"
 ,flagNone ["man"]  (setboolopt "man")  "show the current program's manual with man"
 ,flagNone ["info"] (setboolopt "info") "show the current program's manual with info"
 -- ,flagNone ["browse-args"] (setboolopt "browse-args") "use a web UI to select options and build up a command line"
 ,flagReq  ["debug"]    (\s opts -> Right $ setopt "debug" s opts) "[N]" "show debug output (levels 1-9, default: 1)"
 ,flagNone ["version"] (setboolopt "version") "show version information"
 ]

-- | A hidden flag, just for the hledger executable.
detailedversionflag :: Flag RawOpts
detailedversionflag = flagNone ["version+"] (setboolopt "version+") "show version information with extra detail"

-- | Common input-related flags: --file, --rules-file, --alias...
inputflags :: [Flag RawOpts]
inputflags = [
  flagReq ["file","f"]  (\s opts -> Right $ setopt "file" s opts) "FILE" "use a different input file. For stdin, use -"
 ,flagReq ["rules-file"]  (\s opts -> Right $ setopt "rules-file" s opts) "RFILE" "CSV conversion rules file (default: FILE.rules)"
 ,flagReq ["alias"]  (\s opts -> Right $ setopt "alias" s opts)  "OLD=NEW" "display accounts named OLD as NEW"
 ,flagNone ["ignore-assertions","I"] (setboolopt "ignore-assertions") "ignore any balance assertions in the journal"
 ,flagReq ["pivot"]  (\s opts -> Right $ setopt "pivot" s opts)  "TAG" "Replace the accounts of postings with TAG by TAG:<value>"
 ]

-- | Common report-related flags: --period, --cost, etc.
reportflags :: [Flag RawOpts]
reportflags = [
  flagReq  ["begin","b"]     (\s opts -> Right $ setopt "begin" s opts) "DATE" "include postings/txns on or after this date"
 ,flagReq  ["end","e"]       (\s opts -> Right $ setopt "end" s opts) "DATE" "include postings/txns before this date"
 ,flagNone ["daily","D"]     (setboolopt "daily") "multiperiod/multicolumn report by day"
 ,flagNone ["weekly","W"]    (setboolopt "weekly") "multiperiod/multicolumn report by week"
 ,flagNone ["monthly","M"]   (setboolopt "monthly") "multiperiod/multicolumn report by month"
 ,flagNone ["quarterly","Q"] (setboolopt "quarterly") "multiperiod/multicolumn report by quarter"
 ,flagNone ["yearly","Y"]    (setboolopt "yearly") "multiperiod/multicolumn report by year"
 ,flagReq  ["period","p"]    (\s opts -> Right $ setopt "period" s opts) "PERIODEXP" "set start date, end date, and/or reporting interval all at once (overrides the flags above)"
 ,flagNone ["date2"]         (setboolopt "date2") "show, and make -b/-e/-p/date: match, secondary dates instead"

 ,flagNone ["cleared","C"]   (setboolopt "cleared") "include only cleared postings/txns"
 ,flagNone ["pending"]       (setboolopt "pending") "include only pending postings/txns"
 ,flagNone ["uncleared","U"] (setboolopt "uncleared") "include only uncleared (and pending) postings/txns"
 ,flagNone ["real","R"]      (setboolopt "real") "include only non-virtual postings"
 ,flagReq  ["depth"]         (\s opts -> Right $ setopt "depth" s opts) "N" "hide accounts/postings deeper than N"
 ,flagNone ["empty","E"]     (setboolopt "empty") "show items with zero amount, normally hidden"
 ,flagNone ["cost","B"]      (setboolopt "cost") "show amounts in their cost price's commodity"
 ]

-- | Common output-related flags: --output-file, --output-format...
outputflags = [
   flagReq  ["output-file","o"]   (\s opts -> Right $ setopt "output-file" s opts) "FILE[.FMT]" "write output to FILE instead of stdout. A recognised FMT suffix influences the format."
  ,flagReq  ["output-format","O"] (\s opts -> Right $ setopt "output-format" s opts) "FMT" "select the output format. Supported formats: txt, csv."
  ]

argsFlag :: FlagHelp -> Arg RawOpts
argsFlag desc = flagArg (\s opts -> Right $ setopt "args" s opts) desc

generalflagstitle :: String
generalflagstitle = "\nGeneral flags"

generalflagsgroup1, generalflagsgroup2, generalflagsgroup3 :: (String, [Flag RawOpts])
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
        flagNone ["h"] (setboolopt "h") "Show command usage."
        ]
    ,groupHidden = []
    }
 ,modeArgs = ([], Nothing)
 ,modeGroupModes = toGroup []
 }

-- | A basic subcommand mode with the given command name(s).
defCommandMode :: [Name] -> Mode RawOpts
defCommandMode names = defMode {
   modeNames=names
  ,modeValue=[("command", headDef "" names)]
  ,modeArgs = ([], Just $ argsFlag "[PATTERNS]")
  }

-- | A basic subcommand mode suitable for an add-on command.
defAddonCommandMode :: Name -> Mode RawOpts
defAddonCommandMode addon = defMode {
   modeNames = [addon]
  ,modeHelp = fromMaybe "" $ lookup (stripAddonExtension addon) standardAddonsHelp
  ,modeValue=[("command",addon)]
  ,modeGroupFlags = Group {
      groupUnnamed = []
     ,groupHidden = []
     ,groupNamed = [generalflagsgroup1]
     }
  ,modeArgs = ([], Just $ argsFlag "[ARGS]")
  }

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
  ,("addon",  "dummy add-on command for testing")
  ,("addon2", "dummy add-on command for testing")
  ,("addon3", "dummy add-on command for testing")
  ,("addon4", "dummy add-on command for testing")
  ,("addon5", "dummy add-on command for testing")
  ,("addon6", "dummy add-on command for testing")
  ,("addon7", "dummy add-on command for testing")
  ,("addon8", "dummy add-on command for testing")
  ,("addon9", "dummy add-on command for testing")
  ]

-- | Get a mode's usage message as a nicely wrapped string.
showModeUsage :: Mode a -> String
showModeUsage = (showText defaultWrap :: [Text] -> String) .
               (helpText [] HelpFormatDefault :: Mode a -> [Text])

-- | Get the most appropriate documentation topic for a mode.
-- Currently, that is either the hledger, hledger-ui, hledger-web or
-- hledger-api manual.
topicForMode :: Mode a -> Topic
topicForMode m
  | n == "hledger-ui"  = "ui"
  | n == "hledger-web" = "web"
  --  | n == "hledger-api" = lookupDocTxt "api" -- hledger-api uses docopt
  | otherwise          = "cli"
  where n = headDef "" $ modeNames m

-- | Add command aliases to the command's help string.
withAliases :: String -> [String] -> String
s `withAliases` []     = s
s `withAliases` as = s ++ " (" ++ intercalate ", " as ++ ")"
-- s `withAliases` (a:[]) = s ++ " (alias: " ++ a ++ ")"
-- s `withAliases` as     = s ++ " (aliases: " ++ intercalate ", " as ++ ")"


-- help_postscript = [
--   -- "DATES can be Y/M/D or smart dates like \"last month\"."
--   -- ,"PATTERNS are regular"
--   -- ,"expressions which filter by account name.  Prefix a pattern with desc: to"
--   -- ,"filter by transaction description instead, prefix with not: to negate it."
--   -- ,"When using both, not: comes last."
--  ]


-- CliOpts

-- | Command line options, used in the @hledger@ package and above.
-- This is the \"opts\" used throughout hledger CLI code.
-- representing the options and arguments that were provided at
-- startup on the command-line.
data CliOpts = CliOpts {
     rawopts_         :: RawOpts
    ,command_         :: String
    ,file_            :: [FilePath]
    ,rules_file_      :: Maybe FilePath
    ,output_file_     :: Maybe FilePath
    ,output_format_   :: Maybe String
    ,alias_           :: [String]
    ,ignore_assertions_ :: Bool
    ,debug_           :: Int            -- ^ debug level, set by @--debug[=N]@. See also 'Hledger.Utils.debugLevel'.
    ,no_new_accounts_ :: Bool           -- add
    ,width_           :: Maybe String   -- ^ the --width value provided, if any
    ,available_width_ :: Int            -- ^ estimated usable screen width, based on
                                        -- 1. the COLUMNS env var, if set
                                        -- 2. the width reported by the terminal, if supported
                                        -- 3. the default (80)
    ,reportopts_      :: ReportOpts
 } deriving (Show, Data, Typeable)

instance Default CliOpts where def = defcliopts

defcliopts :: CliOpts
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
    def
    def
    defaultWidth
    def

-- | Convert possibly encoded option values to regular unicode strings.
decodeRawOpts :: RawOpts -> RawOpts
decodeRawOpts = map (\(name',val) -> (name', fromSystemString val))

-- | Default width for hledger console output, when not otherwise specified.
defaultWidth :: Int
defaultWidth = 80

-- | Parse raw option string values to the desired final data types.
-- Any relative smart dates will be converted to fixed dates based on
-- today's date. Parsing failures will raise an error.
-- Also records the terminal width, if supported.
rawOptsToCliOpts :: RawOpts -> IO CliOpts
rawOptsToCliOpts rawopts = checkCliOpts <$> do
  ropts <- rawOptsToReportOpts rawopts
  mcolumns <- readMay <$> getEnvSafe "COLUMNS"
  mtermwidth <-
#ifdef mingw32_HOST_OS
    return Nothing
#else
    setupTermFromEnv >>= return . flip getCapability termColumns
    -- XXX Throws a SetupTermError if the terminfo database could not be read, should catch
#endif
  let availablewidth = head $ catMaybes [mcolumns, mtermwidth, Just defaultWidth]
  return defcliopts {
              rawopts_         = rawopts
             ,command_         = stringopt "command" rawopts
             ,file_            = map stripquotes $ listofstringopt "file" rawopts
             ,rules_file_      = maybestringopt "rules-file" rawopts
             ,output_file_     = maybestringopt "output-file" rawopts
             ,output_format_   = maybestringopt "output-format" rawopts
             ,alias_           = map stripquotes $ listofstringopt "alias" rawopts
             ,debug_           = intopt "debug" rawopts
             ,ignore_assertions_ = boolopt "ignore-assertions" rawopts
             ,no_new_accounts_ = boolopt "no-new-accounts" rawopts -- add
             ,width_           = maybestringopt "width" rawopts
             ,available_width_ = availablewidth
             ,reportopts_      = ropts
             }

-- | Do final validation of processed opts, raising an error if there is trouble.
checkCliOpts :: CliOpts -> CliOpts
checkCliOpts opts =
  either optserror (const opts) $ do
    -- XXX move to checkReportOpts or move _format to CliOpts
    case lineFormatFromOpts $ reportopts_ opts of
      Left err -> Left $ "could not parse format option: "++err
      Right _  -> Right ()
  -- XXX check registerWidthsFromOpts opts

-- Currently only used by some extras/ scripts:
-- | Parse hledger CLI options from the command line using the given
-- cmdargs mode, and either return them or, if a help flag is present,
-- print the mode help and exit the program.
getCliOpts :: Mode RawOpts -> IO CliOpts
getCliOpts mode' = do
  args' <- getArgs
  let rawopts = decodeRawOpts $ processValue mode' args'
  opts <- rawOptsToCliOpts rawopts
  debugArgs args' opts
  -- if any (`elem` args) ["--help","-h","-?"]
  when ("h" `inRawOpts` rawopts_ opts) $ putStr (showModeUsage mode') >> exitSuccess
  when ("help" `inRawOpts` rawopts_ opts) $ printHelpForTopic (topicForMode mode') >> exitSuccess
  return opts
  where
    -- | Print debug info about arguments and options if --debug is present.
    debugArgs :: [String] -> CliOpts -> IO ()
    debugArgs args' opts =
      when ("--debug" `elem` args') $ do
        progname' <- getProgName
        putStrLn $ "running: " ++ progname'
        putStrLn $ "raw args: " ++ show args'
        putStrLn $ "processed opts:\n" ++ show opts
        d <- getCurrentDay
        putStrLn $ "search query: " ++ show (queryFromOpts d $ reportopts_ opts)

-- CliOpts accessors

-- | Get the account name aliases from options, if any.
aliasesFromOpts :: CliOpts -> [AccountAlias]
aliasesFromOpts = map (\a -> fromparse $ runParser accountaliasp () ("--alias "++quoteIfNeeded a) $ T.pack a)
                  . alias_

-- | Get the (tilde-expanded, absolute) journal file path from
-- 1. options, 2. an environment variable, or 3. the default.
-- Actually, returns one or more file paths. There will be more
-- than one if multiple -f options were provided.
journalFilePathFromOpts :: CliOpts -> IO [String]
journalFilePathFromOpts opts = do
  f <- defaultJournalPath
  d <- getCurrentDirectory
  mapM (expandPath d) $ ifEmpty (file_ opts) [f]
 where
    ifEmpty [] d = d
    ifEmpty l _ = l

-- | Get the expanded, absolute output file path from options,
-- or the default (-, meaning stdout).
outputFileFromOpts :: CliOpts -> IO FilePath
outputFileFromOpts opts = do
  d <- getCurrentDirectory
  case output_file_ opts of
    Just p  -> expandPath d p
    Nothing -> return "-"

defaultOutputFormat = "txt"

outputFormats =
  [defaultOutputFormat] ++
  ["csv"
  ]

-- | Get the output format from the --output-format option,
-- otherwise from a recognised file extension in the --output-file option,
-- otherwise the default (txt).
outputFormatFromOpts :: CliOpts -> String
outputFormatFromOpts opts =
  case output_format_ opts of
    Just f  -> f
    Nothing ->
      case filePathExtension <$> output_file_ opts of
        Just ext | ext `elem` outputFormats -> ext
        _                                   -> defaultOutputFormat

-- -- | Get the file name without its last extension, from a file path.
-- filePathBaseFileName :: FilePath -> String
-- filePathBaseFileName = fst . splitExtension . snd . splitFileName

-- | Get the last file extension, without the dot, from a file path.
-- May return the null string.
filePathExtension :: FilePath -> String
filePathExtension = dropWhile (=='.') . snd . splitExtension . snd . splitFileName

-- | Get the (tilde-expanded) rules file path from options, if any.
rulesFilePathFromOpts :: CliOpts -> IO (Maybe FilePath)
rulesFilePathFromOpts opts = do
  d <- getCurrentDirectory
  maybe (return Nothing) (fmap Just . expandPath d) $ rules_file_ opts

-- | Get the width in characters to use for console output.
-- This comes from the --width option, or the COLUMNS environment
-- variable, or (on posix platforms) the current terminal width, or 80.
-- Will raise a parse error for a malformed --width argument.
widthFromOpts :: CliOpts -> Int
widthFromOpts CliOpts{width_=Nothing, available_width_=w} = w
widthFromOpts CliOpts{width_=Just s}  =
    case runParser (read `fmap` many1 digit <* eof) () "(unknown)" s of
        Left e   -> optserror $ "could not parse width option: "++show e
        Right w  -> w

-- for register:

-- | Get the width in characters to use for the register command's console output,
-- and also the description column width if specified (following the main width, comma-separated).
-- The widths will be as follows:
-- @
-- no --width flag - overall width is the available width (COLUMNS, or posix terminal width, or 80); description width is unspecified (auto)
-- --width W       - overall width is W, description width is auto
-- --width W,D     - overall width is W, description width is D
-- @
-- Will raise a parse error for a malformed --width argument.
registerWidthsFromOpts :: CliOpts -> (Int, Maybe Int)
registerWidthsFromOpts CliOpts{width_=Nothing, available_width_=w} = (w, Nothing)
registerWidthsFromOpts CliOpts{width_=Just s}  =
    case runParser registerwidthp () "(unknown)" s of
        Left e   -> optserror $ "could not parse width option: "++show e
        Right ws -> ws
    where
        registerwidthp :: Stream [Char] m t => ParsecT [Char] st m (Int, Maybe Int)
        registerwidthp = do
          totalwidth <- read `fmap` many1 digit
          descwidth <- optionMaybe (char ',' >> read `fmap` many1 digit)
          eof
          return (totalwidth, descwidth)

-- | Drop leading components of accounts names as specified by --drop, but only in --flat mode.
maybeAccountNameDrop :: ReportOpts -> AccountName -> AccountName
maybeAccountNameDrop opts a | tree_ opts = a
                            | otherwise  = accountNameDrop (drop_ opts) a

-- for balance, currently:

-- | Parse the format option if provided, possibly returning an error,
-- otherwise get the default value.
lineFormatFromOpts :: ReportOpts -> Either String StringFormat
lineFormatFromOpts = maybe (Right defaultBalanceLineFormat) parseStringFormat . format_

-- | Default line format for balance report: "%20(total)  %2(depth_spacer)%-(account)"
defaultBalanceLineFormat :: StringFormat
defaultBalanceLineFormat = BottomAligned [
      FormatField False (Just 20) Nothing TotalField
    , FormatLiteral "  "
    , FormatField True (Just 2) Nothing DepthSpacerField
    , FormatField True Nothing Nothing AccountField
    ]

-- Other utils

-- | Get the sorted unique precise names and display names of hledger
-- add-ons found in the current user's PATH. The precise names are the
-- add-on's filename with the "hledger-" prefix removed. The display
-- names have the file extension removed also, except when it's needed
-- for disambiguation.
--
-- -- Also when there are exactly two similar names, one with the .hs or
-- -- .lhs extension and the other with the .exe extension or no
-- -- extension - presumably source and compiled versions of a haskell
-- -- script - we exclude the source version.
--
-- This function can return add-on names which shadow built-in command
-- names, but hledger will ignore these.
--
hledgerAddons :: IO ([String],[String])
hledgerAddons = do
  exes <- hledgerExecutablesInPath
  let precisenames = -- concatMap dropRedundant $
                     -- groupBy (\a b -> dropExtension a == dropExtension b) $
                     map stripprefix exes
  let displaynames = concatMap stripext $
                     groupBy (\a b -> dropExtension a == dropExtension b) precisenames
  return (precisenames, displaynames)
  where
    stripprefix = drop (length progname + 1)
    -- dropRedundant [f,f2] | takeExtension f `elem` ["",".exe"] && takeExtension f2 `elem` [".hs",".lhs"] = [f]
    -- dropRedundant fs = fs
    stripext [f] = [dropExtension f]
    stripext fs  = fs

-- | Get the sorted unique filenames of all hledger-* executables in
-- the current user's PATH. Currently these are: files in any of the
-- PATH directories, named hledger-*, with either no extension (and no
-- periods in the name) or one of the addonExtensions.  Limitations:
-- we do not currently check that the file is really a file (not eg a
-- directory) or whether it has execute permission.
hledgerExecutablesInPath :: IO [String]
hledgerExecutablesInPath = do
  pathdirs <- splitOneOf "[:;]" `fmap` getEnvSafe "PATH"
  pathfiles <- concat `fmap` mapM getDirectoryContentsSafe pathdirs
  return $ nub $ sort $ filter isHledgerExeName pathfiles
  -- XXX should exclude directories and files without execute permission.
  -- These will do a stat for each hledger-*, probably ok.
  -- But they need paths, not just filenames
  -- hledgerexes  <- filterM doesFileExist hledgernamed
  -- hledgerexes' <- filterM isExecutable hledgerexes
  -- return hledgerexes

-- isExecutable f = getPermissions f >>= (return . executable)

isHledgerExeName :: String -> Bool
isHledgerExeName = isRight . parsewith hledgerexenamep
    where
      hledgerexenamep = do
        _ <- string progname
        _ <- char '-'
        _ <- many1 (noneOf ".")
        optional (string "." >> choice' (map string addonExtensions))
        eof

stripAddonExtension :: String -> String
stripAddonExtension = regexReplace re "" where re = "\\.(" ++ intercalate "|" addonExtensions ++ ")$"

addonExtensions :: [String]
addonExtensions =
  ["bat"
  ,"com"
  ,"exe"
  ,"hs"
  ,"lhs"
  ,"pl"
  ,"py"
  ,"rb"
  ,"rkt"
  ,"sh"
  -- ,""
  ]

getEnvSafe :: String -> IO String
getEnvSafe v = getEnv v `C.catch` (\(_::C.IOException) -> return "") -- XXX should catch only isDoesNotExistError e

getDirectoryContentsSafe :: FilePath -> IO [String]
getDirectoryContentsSafe d =
    (filter (not . (`elem` [".",".."])) `fmap` getDirectoryContents d) `C.catch` (\(_::C.IOException) -> return [])

-- not used:
-- -- | Print debug info about arguments and options if --debug is present.
-- debugArgs :: [String] -> CliOpts -> IO ()
-- debugArgs args opts =
--   when ("--debug" `elem` args) $ do
--     progname <- getProgName
--     putStrLn $ "running: " ++ progname
--     putStrLn $ "raw args: " ++ show args
--     putStrLn $ "processed opts:\n" ++ show opts
--     d <- getCurrentDay
--     putStrLn $ "search query: " ++ (show $ queryFromOpts d $ reportopts_ opts)

-- tests

tests_Hledger_Cli_CliOptions :: Test
tests_Hledger_Cli_CliOptions = TestList
 [
 ]
