{-|

Common cmdargs modes and flags, a command-line options type, and
related utilities used by hledger commands.

-}

{-# LANGUAGE CPP, ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts, TypeFamilies, OverloadedStrings, PackageImports #-}

module Hledger.Cli.CliOptions (

  -- * cmdargs flags & modes
  helpflags,
  detailedversionflag,
  hiddenflags,
  inputflags,
  reportflags,
  outputflags,
  outputFormatFlag,
  outputFileFlag,
  generalflagsgroup1,
  generalflagsgroup2,
  generalflagsgroup3,
  defMode,
  defCommandMode,
  addonCommandMode,
  hledgerCommandMode,
  argsFlag,
  showModeUsage,
  withAliases,
  likelyExecutablesInPath,
  hledgerExecutablesInPath,

  -- * CLI options
  CliOpts(..),
  defcliopts,
  getHledgerCliOpts,
  decodeRawOpts,
  rawOptsToCliOpts,
  checkCliOpts,
  outputFormats,
  defaultOutputFormat,
  defaultBalanceLineFormat,
  CommandDoc,

  -- possibly these should move into argsToCliOpts
  -- * CLI option accessors
  -- | These do the extra processing required for some options.
  journalFilePathFromOpts,
  rulesFilePathFromOpts,
  outputFileFromOpts,
  outputFormatFromOpts,
  defaultWidth,
  widthFromOpts,
  replaceNumericFlags,
  -- | For register:
  registerWidthsFromOpts,
  -- | For balance:
  lineFormatFromOpts,

  -- * Other utils
  hledgerAddons,
  topicForMode,

--  -- * Convenience re-exports
--  module Data.String.Here,
--  module System.Console.CmdArgs.Explicit,
)
where

import Prelude ()
import "base-compat-batteries" Prelude.Compat
import qualified Control.Exception as C
import Control.Monad (when)
import Data.Char
import Data.Default
import Data.Functor.Identity (Identity)
import "base-compat-batteries" Data.List.Compat
import Data.List.Split (splitOneOf)
import Data.Ord
import Data.Maybe
--import Data.String.Here
-- import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Safe
import System.Console.CmdArgs hiding (Default,def)
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
#ifndef mingw32_HOST_OS
import System.Console.Terminfo
#endif
import System.Directory
import System.Environment
import System.Exit (exitSuccess)
import System.FilePath
import Text.Megaparsec
import Text.Megaparsec.Char

import Hledger
import Hledger.Cli.DocFiles
import Hledger.Cli.Version


-- common cmdargs flags

-- | Common help flags: --help, --debug, --version...
helpflags :: [Flag RawOpts]
helpflags = [
  flagNone ["help","h"] (setboolopt "help") "show general usage (or after CMD, command usage)"
 -- ,flagNone ["browse-args"] (setboolopt "browse-args") "use a web UI to select options and build up a command line"
 ,flagReq  ["debug"]    (\s opts -> Right $ setopt "debug" s opts) "[N]" "show debug output (levels 1-9, default: 1)"
 ,flagNone ["version"] (setboolopt "version") "show version information"
 ]

-- | A hidden flag just for the hledger executable.
detailedversionflag :: Flag RawOpts
detailedversionflag = flagNone ["version+"] (setboolopt "version+") "show version information with extra detail"

-- | Common input-related flags: --file, --rules-file, --alias...
inputflags :: [Flag RawOpts]
inputflags = [
  flagReq  ["file","f"]      (\s opts -> Right $ setopt "file" s opts) "FILE" "use a different input file. For stdin, use - (default: $LEDGER_FILE or $HOME/.hledger.journal)"
 ,flagReq  ["rules-file"]    (\s opts -> Right $ setopt "rules-file" s opts) "RFILE" "CSV conversion rules file (default: FILE.rules)"
 ,flagReq  ["separator"]     (\s opts -> Right $ setopt "separator" s opts) "SEP" "CSV separator (default: ,)"
 ,flagReq  ["alias"]         (\s opts -> Right $ setopt "alias" s opts)  "OLD=NEW" "rename accounts named OLD to NEW"
 ,flagNone ["anon"]          (setboolopt "anon") "anonymize accounts and payees"
 ,flagReq  ["pivot"]         (\s opts -> Right $ setopt "pivot" s opts)  "TAGNAME" "use some other field/tag for account names"
 ,flagNone ["ignore-assertions","I"] (setboolopt "ignore-assertions") "ignore any balance assertions"
 ]

-- | Common report-related flags: --period, --cost, etc.
reportflags :: [Flag RawOpts]
reportflags = [

  -- report period & interval
  flagReq  ["begin","b"]     (\s opts -> Right $ setopt "begin" s opts) "DATE" "include postings/txns on or after this date"
 ,flagReq  ["end","e"]       (\s opts -> Right $ setopt "end" s opts) "DATE" "include postings/txns before this date"
 ,flagNone ["daily","D"]     (setboolopt "daily") "multiperiod/multicolumn report by day"
 ,flagNone ["weekly","W"]    (setboolopt "weekly") "multiperiod/multicolumn report by week"
 ,flagNone ["monthly","M"]   (setboolopt "monthly") "multiperiod/multicolumn report by month"
 ,flagNone ["quarterly","Q"] (setboolopt "quarterly") "multiperiod/multicolumn report by quarter"
 ,flagNone ["yearly","Y"]    (setboolopt "yearly") "multiperiod/multicolumn report by year"
 ,flagReq  ["period","p"]    (\s opts -> Right $ setopt "period" s opts) "PERIODEXP" "set start date, end date, and/or report interval all at once"
 ,flagNone ["date2"]         (setboolopt "date2") "match the secondary date instead. See command help for other effects. (--effective, --aux-date also accepted)"  -- see also hiddenflags

  -- status/realness/depth/zero filters
 ,flagNone ["unmarked","U"]  (setboolopt "unmarked") "include only unmarked postings/txns (can combine with -P or -C)"
 ,flagNone ["pending","P"]   (setboolopt "pending") "include only pending postings/txns"
 ,flagNone ["cleared","C"]   (setboolopt "cleared") "include only cleared postings/txns"
 ,flagNone ["real","R"]      (setboolopt "real") "include only non-virtual postings"
 ,flagReq  ["depth"]         (\s opts -> Right $ setopt "depth" s opts) "NUM" "(or -NUM): hide accounts/postings deeper than this"
 ,flagNone ["empty","E"]     (setboolopt "empty") "show items with zero amount, normally hidden (and vice-versa in hledger-ui/hledger-web)"

  -- valuation
 ,flagNone ["B","cost"]      (setboolopt "B")
   "show amounts converted to their cost, using the transaction price. Equivalent to --value=cost."
 ,flagNone ["V","market"]    (setboolopt "V")
   (unwords
     ["show amounts converted to current market value (single period reports)"
     ,"or period-end market value (multiperiod reports) in their default valuation commodity."
     ,"Equivalent to --value=now / --value=end."
     ])
 ,flagReq ["X","exchange"]   (\s opts -> Right $ setopt "X" s opts) "COMM"
   (unwords
     ["show amounts converted to current (single period reports)"
     ,"or period-end (multiperiod reports) market value in the specified commodity."
     ,"Equivalent to --value=now,COMM / --value=end,COMM."
     ])
 ,flagReq  ["value"]         (\s opts -> Right $ setopt "value" s opts) "TYPE[,COMM]"
   (unlines
     ["TYPE is cost, end, now or YYYY-MM-DD."
     ,"COMM is an optional commodity symbol."
     ,"Shows amounts converted to:"
     ,"- cost using transaction prices, then optionally to COMM using period-end market prices"
     ,"- period-end market value, in default valuation commodity or COMM"
     ,"- current market value, in default valuation commodity or COMM"
     ,"- market value on the given date, in default valuation commodity or COMM"
     ])

  -- generated postings/transactions
 ,flagNone ["auto"]          (setboolopt "auto") "apply automated posting rules to modify transactions"
 ,flagNone ["forecast"]      (setboolopt "forecast") "apply periodic transaction rules to generate future transactions, to 6 months from now or report end date"

 ]

-- | Common flags that are accepted but not shown in --help,
-- such as --effective, --aux-date.
hiddenflags :: [Flag RawOpts]
hiddenflags = [
  flagNone ["effective","aux-date"] (setboolopt "date2") "Ledger-compatible aliases for --date2"
 ]

-- | Common output-related flags: --output-file, --output-format...
outputflags = [outputFormatFlag, outputFileFlag]
outputFormatFlag = flagReq  ["output-format","O"] (\s opts -> Right $ setopt "output-format" s opts) "FMT" "select the output format. Supported formats:\ntxt, csv, html."
outputFileFlag   = flagReq  ["output-file","o"]   (\s opts -> Right $ setopt "output-file" s opts) "FILE" "write output to FILE. A file extension matching one of the above formats selects that format."

argsFlag :: FlagHelp -> Arg RawOpts
argsFlag desc = flagArg (\s opts -> Right $ setopt "args" s opts) desc

generalflagstitle :: String
generalflagstitle = "\nGeneral flags"

generalflagsgroup1, generalflagsgroup2, generalflagsgroup3 :: (String, [Flag RawOpts])
generalflagsgroup1 = (generalflagstitle, inputflags ++ reportflags ++ helpflags)
generalflagsgroup2 = (generalflagstitle, inputflags ++ helpflags)
generalflagsgroup3 = (generalflagstitle, helpflags)

-- cmdargs mode constructors

-- | An empty cmdargs mode to use as a template.
-- Modes describe the top-level command, ie the program, or a subcommand,
-- telling cmdargs how to parse a command line and how to
-- generate the command's usage text.
defMode :: Mode RawOpts
defMode = Mode {
  modeNames       = []            -- program/command name(s)
 ,modeHelp        = ""            -- short help for this command
 ,modeHelpSuffix  = []            -- text displayed after the usage
 ,modeGroupFlags  = Group {       -- description of flags accepted by the command
    groupNamed   = []             --  named groups of flags
   ,groupUnnamed = []             --  ungrouped flags
   ,groupHidden  = []             --  flags not displayed in the usage
   }
 ,modeArgs        = ([], Nothing) -- description of arguments accepted by the command
 ,modeValue       = []            -- value returned when this mode is used to parse a command line
 ,modeCheck       = Right         -- whether the mode's value is correct
 ,modeReform      = const Nothing -- function to convert the value back to a command line arguments
 ,modeExpandAt    = True          -- expand @ arguments for program ?
 ,modeGroupModes  = toGroup []    -- sub-modes
 }

-- | A cmdargs mode suitable for a hledger built-in command
-- with the given names (primary name + optional aliases).
-- The usage message shows [QUERY] as argument.
defCommandMode :: [Name] -> Mode RawOpts
defCommandMode names = defMode {
   modeNames=names
  ,modeGroupFlags  = Group {
     groupNamed   = []
    ,groupUnnamed = [
       flagNone ["help"] (setboolopt "help") "Show usage."
      -- ,flagNone ["help"] (setboolopt "help") "Show long help."
      ]
    ,groupHidden  = []             --  flags not displayed in the usage
    }
  ,modeArgs = ([], Just $ argsFlag "[QUERY]")
  ,modeValue=[("command", headDef "" names)]
  }

-- | A cmdargs mode representing the hledger add-on command with the
-- given name, providing hledger's common input/reporting/help flags.
-- Just used when invoking addons.
addonCommandMode :: Name -> Mode RawOpts
addonCommandMode name = (defCommandMode [name]) {
   modeHelp = ""
     -- XXX not needed ?
     -- fromMaybe "" $ lookup (stripAddonExtension name) [
     --   ("addon"        , "dummy add-on command for testing")
     --  ,("addon2"       , "dummy add-on command for testing")
     --  ,("addon3"       , "dummy add-on command for testing")
     --  ,("addon4"       , "dummy add-on command for testing")
     --  ,("addon5"       , "dummy add-on command for testing")
     --  ,("addon6"       , "dummy add-on command for testing")
     --  ,("addon7"       , "dummy add-on command for testing")
     --  ,("addon8"       , "dummy add-on command for testing")
     --  ,("addon9"       , "dummy add-on command for testing")
     --  ]
  ,modeGroupFlags = Group {
      groupUnnamed = []
     ,groupHidden  = hiddenflags
     ,groupNamed   = [generalflagsgroup1]
     }
  }

-- | A command's documentation. Used both as part of CLI help, and as
-- part of the hledger manual. See parseCommandDoc.
type CommandDoc = String

-- | Build a cmdarg mode for a hledger command,
-- from a help template and flag/argument specifications.
-- Reduces boilerplate a little, though the complicated cmdargs
-- flag and argument specs are still required.
hledgerCommandMode :: CommandDoc -> [Flag RawOpts] -> [(String, [Flag RawOpts])]
  -> [Flag RawOpts] -> ([Arg RawOpts], Maybe (Arg RawOpts)) -> Mode RawOpts
hledgerCommandMode doc unnamedflaggroup namedflaggroups hiddenflaggroup argsdescr =
  case parseCommandDoc doc of
    Nothing -> error' $ "Could not parse command doc:\n"++doc++"\n"
    Just (names, shorthelp, longhelplines) ->
      (defCommandMode names) {
         modeHelp        = shorthelp
        ,modeHelpSuffix  = longhelplines
        ,modeGroupFlags  = Group {
            groupUnnamed = unnamedflaggroup
           ,groupNamed   = namedflaggroups
           ,groupHidden  = hiddenflaggroup
           }
        ,modeArgs        = argsdescr
        }

-- | Parse a command's documentation, as follows:
--
-- - First line: the command name then any aliases, as one or more space or comma-separated words
--
-- - Second line to a line containing just _FLAGS_, or the end: the short help
--
-- - Any lines after _FLAGS_: the long help (split into lines for cmdargs)
--
-- The CLI help displays the short help, then the cmdargs-generated
-- flags list, then the long help (which some day we might make
-- optional again).  The manual displays the short help followed by
-- the long help, with no flags list.
--
parseCommandDoc :: CommandDoc -> Maybe ([Name], String, [String])
parseCommandDoc t =
  case lines t of
    [] -> Nothing
    (l:ls) -> Just (names, shorthelp, longhelplines)
      where
        names = words $ map (\c -> if c `elem` [',','\\'] then ' ' else c) l
        (shorthelpls, longhelpls) = break (== "_FLAGS_") ls
        shorthelp = unlines $ reverse $ dropWhile null $ reverse shorthelpls
        longhelplines = dropWhile null $ drop 1 longhelpls

-- | Get a mode's usage message as a nicely wrapped string.
showModeUsage :: Mode a -> String
showModeUsage = (showText defaultWrap :: [Text] -> String) .
               (helpText [] HelpFormatDefault :: Mode a -> [Text])

-- | Get the most appropriate documentation topic for a mode.
-- Currently, that is either the hledger, hledger-ui or hledger-web
-- manual.
topicForMode :: Mode a -> Topic
topicForMode m
  | n == "hledger-ui"  = "ui"
  | n == "hledger-web" = "web"
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
    ,inputopts_       :: InputOpts
    ,reportopts_      :: ReportOpts
    ,output_file_     :: Maybe FilePath
    ,output_format_   :: Maybe String
    ,debug_           :: Int            -- ^ debug level, set by @--debug[=N]@. See also 'Hledger.Utils.debugLevel'.
    ,no_new_accounts_ :: Bool           -- add
    ,width_           :: Maybe String   -- ^ the --width value provided, if any
    ,available_width_ :: Int            -- ^ estimated usable screen width, based on
                                        -- 1. the COLUMNS env var, if set
                                        -- 2. the width reported by the terminal, if supported
                                        -- 3. the default (80)
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
    defaultWidth

-- | Convert possibly encoded option values to regular unicode strings.
decodeRawOpts :: RawOpts -> RawOpts
decodeRawOpts = map (\(name',val) -> (name', fromSystemString val))

-- | Default width for hledger console output, when not otherwise specified.
defaultWidth :: Int
defaultWidth = 80

-- | Replace any numeric flags (eg -2) with their long form (--depth 2),
-- as I'm guessing cmdargs doesn't support this directly.
replaceNumericFlags :: [String] -> [String]
replaceNumericFlags = map replace
  where
    replace ('-':ds) | not (null ds) && all isDigit ds = "--depth="++ds
    replace s = s

-- | Parse raw option string values to the desired final data types.
-- Any relative smart dates will be converted to fixed dates based on
-- today's date. Parsing failures will raise an error.
-- Also records the terminal width, if supported.
rawOptsToCliOpts :: RawOpts -> IO CliOpts
rawOptsToCliOpts rawopts = checkCliOpts <$> do
  let iopts = rawOptsToInputOpts rawopts
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
             ,file_            = listofstringopt "file" rawopts
             ,inputopts_       = iopts
             ,reportopts_      = ropts
             ,output_file_     = maybestringopt "output-file" rawopts
             ,output_format_   = maybestringopt "output-format" rawopts
             ,debug_           = intopt "debug" rawopts
             ,no_new_accounts_ = boolopt "no-new-accounts" rawopts -- add
             ,width_           = maybestringopt "width" rawopts
             ,available_width_ = availablewidth
             }

-- | Do final validation of processed opts, raising an error if there is trouble.
checkCliOpts :: CliOpts -> CliOpts
checkCliOpts opts =
  either usageError (const opts) $ do
    -- XXX move to checkReportOpts or move _format to CliOpts
    case lineFormatFromOpts $ reportopts_ opts of
      Left err -> Left $ "could not parse format option: "++err
      Right _  -> Right ()
  -- XXX check registerWidthsFromOpts opts

-- | A helper for addon commands: this parses options and arguments from
-- the current command line using the given hledger-style cmdargs mode,
-- and returns a CliOpts. Or, with --help or -h present, it prints
-- long or short help, and exits the program.
-- When --debug is present, also prints some debug output.
-- Note this is not used by the main hledger executable.
--
-- The help texts are generated from the mode.
-- Long help includes the full usage description generated by cmdargs
-- (including all supported options), framed by whatever pre- and postamble
-- text the mode specifies. It's intended that this forms a complete
-- help document or manual.
--
-- Short help is a truncated version of the above: the preamble and
-- the first part of the usage, up to the first line containing "flags:"
-- (normally this marks the start of the common hledger flags);
-- plus a mention of --help and the (presumed supported) common
-- hledger options not displayed.
--
-- Tips:
-- Empty lines in the pre/postamble are removed by cmdargs;
-- add a space character to preserve them.
--
getHledgerCliOpts :: Mode RawOpts -> IO CliOpts
getHledgerCliOpts mode' = do
  args' <- getArgs >>= expandArgsAt
  let rawopts = either usageError decodeRawOpts $ process mode' args'
  opts <- rawOptsToCliOpts rawopts
  debugArgs args' opts
  when ("help" `inRawOpts` rawopts_ opts) $ putStr shorthelp >> exitSuccess
  -- when ("help" `inRawOpts` rawopts_ opts) $ putStr longhelp  >> exitSuccess
  return opts
  where
    longhelp = showModeUsage mode'
    shorthelp =
      unlines $
        (reverse $ dropWhile null $ reverse $ takeWhile (not . ("flags:" `isInfixOf`)) $ lines longhelp)
        ++
        [""
        ,"  See also hledger -h for general hledger options."
        ]
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

-- | Get the (tilde-expanded, absolute) journal file path from
-- 1. options, 2. an environment variable, or 3. the default.
-- Actually, returns one or more file paths. There will be more
-- than one if multiple -f options were provided.
-- File paths can have a READER: prefix naming a reader/data format.
journalFilePathFromOpts :: CliOpts -> IO [String]
journalFilePathFromOpts opts = do
  f <- defaultJournalPath
  d <- getCurrentDirectory
  case file_ opts of
    [] -> return [f]
    fs -> mapM (expandPathPreservingPrefix d) fs

expandPathPreservingPrefix :: FilePath -> PrefixedFilePath -> IO PrefixedFilePath
expandPathPreservingPrefix d prefixedf = do
  let (p,f) = splitReaderPrefix prefixedf
  f' <- expandPath d f
  return $ case p of
    Just p  -> p ++ ":" ++ f'
    Nothing -> f'

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
  ,"html"
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
  maybe (return Nothing) (fmap Just . expandPath d) $ mrules_file_ $ inputopts_ opts

-- | Get the width in characters to use for console output.
-- This comes from the --width option, or the COLUMNS environment
-- variable, or (on posix platforms) the current terminal width, or 80.
-- Will raise a parse error for a malformed --width argument.
widthFromOpts :: CliOpts -> Int
widthFromOpts CliOpts{width_=Nothing, available_width_=w} = w
widthFromOpts CliOpts{width_=Just s}  =
    case runParser (read `fmap` some digitChar <* eof :: ParsecT Void String Identity Int) "(unknown)" s of
        Left e   -> usageError $ "could not parse width option: "++show e
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
    case runParser registerwidthp "(unknown)" s of
        Left e   -> usageError $ "could not parse width option: "++show e
        Right ws -> ws
    where
        registerwidthp :: (Stream s, Char ~ Token s) => ParsecT Void s m (Int, Maybe Int)
        registerwidthp = do
          totalwidth <- read `fmap` some digitChar
          descwidth <- optional (char ',' >> read `fmap` some digitChar)
          eof
          return (totalwidth, descwidth)

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

-- | Get the sorted unique canonical names of hledger addon commands
-- found in the current user's PATH. These are used in command line
-- parsing and to display the commands list.
--
-- Canonical addon names are the filenames of hledger-* executables in
-- PATH, without the "hledger-" prefix, and without the file extension
-- except when it's needed for disambiguation (see below).
--
-- When there are exactly two versions of an executable (same base
-- name, different extensions) that look like a source and compiled
-- pair (one has .exe, .com, or no extension), the source version will
-- be excluded (even if it happens to be newer). When there are three
-- or more versions (or two versions that don't look like a
-- source/compiled pair), they are all included, with file extensions
-- intact.
--
hledgerAddons :: IO [String]
hledgerAddons = do
  -- past bug generator
  as1 <- hledgerExecutablesInPath                                  -- ["hledger-check","hledger-check-dates","hledger-check-dates.hs","hledger-check.hs","hledger-check.py"]
  let as2 = map stripPrognamePrefix as1                            -- ["check","check-dates","check-dates.hs","check.hs","check.py"]
  let as3 = sortBy (comparing takeBaseName) as2                    -- ["check","check.hs","check.py","check-dates","check-dates.hs"]
  let as4 = groupBy (\a b -> takeBaseName a == takeBaseName b) as3 -- [["check","check.hs","check.py"],["check-dates","check-dates.hs"]]
  let as5 = concatMap dropRedundantSourceVersion as4               -- ["check","check.hs","check.py","check-dates"]
  return as5

stripPrognamePrefix = drop (length progname + 1)

dropRedundantSourceVersion [f,g]
  | takeExtension f `elem` compiledExts = [f]
  | takeExtension g `elem` compiledExts = [g]
dropRedundantSourceVersion fs = fs

compiledExts = ["",".com",".exe"]


-- | Get all sorted unique filenames in the current user's PATH.
-- We do not currently filter out non-file objects or files without execute permission.
likelyExecutablesInPath :: IO [String]
likelyExecutablesInPath = do
  pathdirs <- splitOneOf "[:;]" `fmap` getEnvSafe "PATH"
  pathfiles <- concat `fmap` mapM getDirectoryContentsSafe pathdirs
  return $ nub $ sort pathfiles
  -- exclude directories and files without execute permission.
  -- These will do a stat for each hledger-*, probably ok.
  -- But they need paths, not just filenames
  -- exes'  <- filterM doesFileExist exe'
  -- exes'' <- filterM isExecutable exes'
  -- return exes''

-- | Get the sorted unique filenames of all hledger-* executables in
-- the current user's PATH. These are files in any of the PATH directories,
-- named hledger-*, with either no extension (and no periods in the name)
-- or one of the addonExtensions.
-- We do not currently filter out non-file objects or files without execute permission.
hledgerExecutablesInPath :: IO [String]
hledgerExecutablesInPath = filter isHledgerExeName <$> likelyExecutablesInPath

-- isExecutable f = getPermissions f >>= (return . executable)

isHledgerExeName :: String -> Bool
isHledgerExeName = isRight . parsewith hledgerexenamep . T.pack
    where
      hledgerexenamep = do
        _ <- string $ T.pack progname
        _ <- char '-'
        _ <- some $ noneOf ['.']
        optional (string "." >> choice' (map (string . T.pack) addonExtensions))
        eof

-- stripAddonExtension :: String -> String
-- stripAddonExtension = regexReplace re "" where re = "\\.(" ++ intercalate "|" addonExtensions ++ ")$"

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

