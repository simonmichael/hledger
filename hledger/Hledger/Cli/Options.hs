{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-|

Common cmdargs modes and flags, a command-line options type, and
related utilities used by hledger commands.

-}

module Hledger.Cli.Options (

  -- * cmdargs flags & modes
  helpflags,
  inputflags,
  reportflags,
  generalflagsgroup1,
  generalflagsgroup2,
  generalflagsgroup3,
  defMode,
  defCommandMode,
  defAddonCommandMode,
  argsFlag,
  showModeHelp,
  withAliases,
  
  -- * CLI options
  CliOpts(..),
  defcliopts,
  getCliOpts,
  decodeRawOpts,
  rawOptsToCliOpts,
  checkCliOpts,

  -- possibly these should move into argsToCliOpts
  -- * CLI option accessors
  -- | These do the extra processing required for some options.
  aliasesFromOpts,
  journalFilePathFromOpts,
  rulesFilePathFromOpts,
  -- | For register:
  OutputWidth(..),
  Width(..),
  defaultWidth,
  defaultWidthWithFlag,
  widthFromOpts,
  -- | For balance:
  formatFromOpts,

  -- * Other utils
  hledgerAddons,

  -- * Tests
  tests_Hledger_Cli_Options

) 
where
  
import qualified Control.Exception as C
import Control.Monad (when)
import Data.List
import Data.Maybe
import Safe
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
import System.Directory
import System.Environment
import System.Exit (exitSuccess)
import System.FilePath
import Test.HUnit
import Text.ParserCombinators.Parsec as P

import Hledger
import Hledger.Data.OutputFormat as OutputFormat
import Hledger.Cli.Version


-- common cmdargs flags

-- | Common help flags: --help, --debug, --version...
helpflags :: [Flag RawOpts]
helpflags = [
  flagNone ["help","h","?"] (setboolopt "help") "Display general help or (with --help after COMMAND) command help."
 -- ,flagNone ["browse-args"] (setboolopt "browse-args") "use a web UI to select options and build up a command line"
 ,flagOpt "1" ["debug"] (\s opts -> Right $ setopt "debug" s opts) "N" "Show debug output (optional argument sets debug level)"
 ,flagNone ["version"] (setboolopt "version") "Print version information"
 ]

-- | Common input-related flags: --file, --rules-file, --alias...
inputflags :: [Flag RawOpts]
inputflags = [
  flagReq ["file","f"]  (\s opts -> Right $ setopt "file" s opts) "FILE" "use a different journal file; - means stdin"
 ,flagReq ["rules-file"]  (\s opts -> Right $ setopt "rules-file" s opts) "RULESFILE" "conversion rules for CSV (default: FILE.rules)"
 ,flagReq ["alias"]  (\s opts -> Right $ setopt "alias" s opts)  "ACCT=ALIAS" "convert ACCT's name to ALIAS"
 ]

-- | Common report-related flags: --period, --cost, etc.
reportflags :: [Flag RawOpts]
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
 ,flagNone ["date2","aux-date"] (\opts -> setboolopt "date2" opts) "use transactions' secondary dates, if any"
 ,flagNone ["empty","E"]     (\opts -> setboolopt "empty" opts) "show empty/zero things which are normally elided"
 ,flagNone ["real","R"]      (\opts -> setboolopt "real" opts) "report only on real (non-virtual) transactions"
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
        flagNone ["help","h","?"] (setboolopt "help") "Display command help."
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

-- | Get a mode's help message as a nicely wrapped string.
showModeHelp :: Mode a -> String
showModeHelp = (showText defaultWrap :: [Text] -> String) . 
               (helpText [] HelpFormatDefault :: Mode a -> [Text])

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
-- representing the options that arguments that were provided at
-- startup on the command-line.
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

-- | Convert possibly encoded option values to regular unicode strings.
decodeRawOpts :: RawOpts -> RawOpts
decodeRawOpts = map (\(name,val) -> (name, fromSystemString val))

-- | Parse raw option string values to the desired final data types.
-- Any relative smart dates will be converted to fixed dates based on
-- today's date. Parsing failures will raise an error.
rawOptsToCliOpts :: RawOpts -> IO CliOpts
rawOptsToCliOpts rawopts = do
  ropts <- rawOptsToReportOpts rawopts
  return defcliopts {
              rawopts_         = rawopts
             ,command_         = stringopt "command" rawopts
             ,file_            = maybestringopt "file" rawopts
             ,rules_file_      = maybestringopt "rules-file" rawopts
             ,alias_           = map stripquotes $ listofstringopt "alias" rawopts
             ,debug_           = intopt "debug" rawopts
             ,no_new_accounts_ = boolopt "no-new-accounts" rawopts -- add
             ,width_           = maybestringopt "width" rawopts    -- register
             ,reportopts_      = ropts
             }
  
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

-- Currently only used by some extras/ scripts:
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
  where
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

-- CliOpts accessors

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

-- for balance, currently:

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

-- for register:

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
defaultWidth :: Int
defaultWidth = 80

-- | Width of hledger console output when the -w flag is used with no value.
defaultWidthWithFlag :: Int
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
                     groupBy (\a b -> dropExtension a == dropExtension b) $
                     precisenames
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
  pathdirs <- regexSplit "[:;]" `fmap` getEnvSafe "PATH"
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
        string progname
        char '-'
        many1 (noneOf ".")
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
getEnvSafe v = getEnv v `C.catch` (\(_::C.IOException) -> return "")

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

tests_Hledger_Cli_Options :: Test
tests_Hledger_Cli_Options = TestList
 [
 ]
