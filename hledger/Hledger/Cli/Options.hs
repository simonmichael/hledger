{-|

Command-line options for the hledger program, and option-parsing utilities.

-}

module Hledger.Cli.Options
where
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
import Text.Parsec
import Text.Printf

import Hledger.Cli.Format as Format
import Hledger.Cli.Reports
import Hledger.Cli.Version
import Hledger.Data
import Hledger.Read
import Hledger.Utils


progname = "hledger"
progversion = progversionstr progname

-- 1. cmdargs mode and flag definitions, for the main and subcommand modes.
-- Flag values are parsed initially to simple RawOpts to permit reuse.

type RawOpts = [(String,String)]

defmode :: Mode RawOpts
defmode =   Mode {
  modeNames = []
 ,modeHelp = ""
 ,modeHelpSuffix = []
 ,modeValue = []
 ,modeCheck = Right
 ,modeReform = const Nothing
 ,modeGroupFlags = toGroup []
 ,modeArgs = ([], Nothing)
 ,modeGroupModes = toGroup []
 }

mainmode addons = defmode {
  modeNames = [progname]
 ,modeHelp = "run the specified hledger command. hledger COMMAND --help for more detail. \nIn general, COMMAND should precede OPTIONS."
 ,modeHelpSuffix = [""]
 ,modeGroupFlags = Group {
     groupUnnamed = helpflags
    ,groupHidden = [flagNone ["binary-filename"] (setboolopt "binary-filename") "show the download filename for this executable, and exit"]
    ,groupNamed = []
    }
 ,modeArgs = ([], Just mainargsflag)
 ,modeGroupModes = Group {
     groupUnnamed = [
     ]
    ,groupHidden = [
     ]
    ,groupNamed = [
      ("Misc commands", [
        addmode
       ,convertmode
       ,testmode
       ])
     ,("\nReport commands", [
        accountsmode
       ,entriesmode
       ,postingsmode
       -- ,transactionsmode
       ,activitymode
       ,statsmode
       ])
     ]
     ++ case addons of [] -> []
                       cs -> [("\nAdd-on commands found", map addonmode cs)]
    }
 }

addonmode name = defmode {
  modeNames = [name]
 ,modeHelp = printf "[-- OPTIONS]   run the %s-%s program" progname name
 ,modeValue=[("command",name)]
 ,modeGroupFlags = Group {
     groupUnnamed = []
    ,groupHidden = []
    ,groupNamed = [(generalflagstitle, generalflags1)]
    }
 ,modeArgs = ([], Just addonargsflag)
 }

help_postscript = [
  -- "DATES can be Y/M/D or smart dates like \"last month\"."
  -- ,"PATTERNS are regular"
  -- ,"expressions which filter by account name.  Prefix a pattern with desc: to"
  -- ,"filter by transaction description instead, prefix with not: to negate it."
  -- ,"When using both, not: comes last."
 ]

generalflagstitle = "\nGeneral flags"
generalflags1 = fileflags ++ reportflags ++ helpflags
generalflags2 = fileflags ++ helpflags
generalflags3 = helpflags

fileflags = [
  flagReq ["file","f"]  (\s opts -> Right $ setopt "file" s opts) "FILE" "use a different journal file; - means stdin"
 ,flagReq ["alias"]  (\s opts -> Right $ setopt "alias" s opts)  "ACCT=ALIAS" "display ACCT's name as ALIAS in reports"
 ]

reportflags = [
  flagReq  ["begin","b"]     (\s opts -> Right $ setopt "begin" s opts) "DATE" "report on transactions on or after this date"
 ,flagReq  ["end","e"]       (\s opts -> Right $ setopt "end" s opts) "DATE" "report on transactions before this date"
 ,flagReq  ["period","p"]    (\s opts -> Right $ setopt "period" s opts) "PERIODEXPR" "report on transactions during the specified period and/or with the specified reporting interval"
 ,flagNone ["daily","D"]     (\opts -> setboolopt "daily" opts) "report by day"
 ,flagNone ["weekly","W"]    (\opts -> setboolopt "weekly" opts) "report by week"
 ,flagNone ["monthly","M"]   (\opts -> setboolopt "monthly" opts) "report by month"
 ,flagNone ["quarterly","Q"] (\opts -> setboolopt "quarterly" opts) "report by quarter"
 ,flagNone ["yearly","Y"]    (\opts -> setboolopt "yearly" opts) "report by year"
 ,flagNone ["cleared","C"]   (\opts -> setboolopt "cleared" opts) "report only on cleared transactions"
 ,flagNone ["uncleared","U"] (\opts -> setboolopt "uncleared" opts) "report only on uncleared transactions"
 ,flagNone ["cost","B"]      (\opts -> setboolopt "cost" opts) "report cost of commodities"
 ,flagReq  ["depth"]         (\s opts -> Right $ setopt "depth" s opts) "N" "hide accounts/transactions deeper than this"
 ,flagReq  ["display","d"]   (\s opts -> Right $ setopt "display" s opts) "DISPLAYEXPR" "show only transactions matching the expr, which is 'dOP[DATE]' where OP is <, <=, =, >=, >"
 ,flagNone ["effective"]     (\opts -> setboolopt "effective" opts) "use transactions' effective dates, if any"
 ,flagNone ["empty","E"]     (\opts -> setboolopt "empty" opts) "show empty/zero things which are normally elided"
 ,flagNone ["real","R"]      (\opts -> setboolopt "real" opts) "report only on real (non-virtual) transactions"
 ]

helpflags = [
  flagHelpSimple (setboolopt "help")
 ,flagNone ["debug"] (setboolopt "debug") "Show extra debug output"
 ,flagVersion (setboolopt "version")
 ]

mainargsflag = flagArg f ""
    where f s opts = let as = words' s
                         cmd = headDef "" as
                         args = drop (length cmd + 1) s
                     in Right $ setopt "command" cmd $ setopt "args" args opts

commandargsflag = flagArg (\s opts -> Right $ setopt "args" s opts) "[PATTERNS]"

addonargsflag = flagArg (\s opts -> Right $ setopt "args" s opts) "[ARGS]"

commandmode names = defmode {modeNames=names, modeValue=[("command",headDef "" names)]}

addmode = (commandmode ["add"]) {
  modeHelp = "prompt for new transactions and append them to the journal"
 ,modeHelpSuffix = ["Defaults come from previous similar transactions; use query patterns to restrict these."]
 ,modeArgs = ([], Just commandargsflag)
 ,modeGroupFlags = Group {
     groupUnnamed = [
      flagNone ["no-new-accounts"]  (\opts -> setboolopt "no-new-accounts" opts) "don't allow creating new accounts"
     ]
    ,groupHidden = []
    ,groupNamed = [(generalflagstitle, generalflags2)]
    }
 }

convertmode = (commandmode ["convert"]) {
  modeValue = [("command","convert")]
 ,modeHelp = "show the specified CSV file as hledger journal entries"
 ,modeArgs = ([], Just $ flagArg (\s opts -> Right $ setopt "args" s opts) "[CSVFILE]")
 ,modeGroupFlags = Group {
     groupUnnamed = [
      flagReq ["rules-file"]  (\s opts -> Right $ setopt "rules-file" s opts) "FILE" "rules file to use (default: CSVFILE.rules)"
     ]
    ,groupHidden = []
    ,groupNamed = [(generalflagstitle, generalflags3)]
    }
 }

testmode = (commandmode ["test"]) {
  modeHelp = "run self-tests, or just the ones matching REGEXPS"
 ,modeArgs = ([], Just $ flagArg (\s opts -> Right $ setopt "args" s opts) "[REGEXPS]")
 ,modeGroupFlags = Group {
     groupUnnamed = []
    ,groupHidden = []
    ,groupNamed = [(generalflagstitle, generalflags3)]
    }
 }

accountsmode = (commandmode ["balance","accounts"]) {
  modeHelp = "(or accounts) show matched accounts and their balances"
 ,modeArgs = ([], Just commandargsflag)
 ,modeGroupFlags = Group {
     groupUnnamed = [
      flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show full account names, unindented"
     ,flagReq ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "with --flat, omit this many leading account name components"
     ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "use this custom line format"
     ,flagNone ["no-elide"] (\opts -> setboolopt "no-elide" opts) "no eliding at all, stronger than --empty"
     ,flagNone ["no-total"] (\opts -> setboolopt "no-total" opts) "don't show the final total"
     ]
    ,groupHidden = []
    ,groupNamed = [(generalflagstitle, generalflags1)]
    }
 }

entriesmode = (commandmode ["print","entries"]) {
  modeHelp = "(or entries) show matched journal entries"
 ,modeArgs = ([], Just commandargsflag)
 ,modeGroupFlags = Group {
     groupUnnamed = []
    ,groupHidden = []
    ,groupNamed = [(generalflagstitle, generalflags1)]
    }
 }

postingsmode = (commandmode ["register","postings"]) {
  modeHelp = "(or postings) show matched postings and running total"
 ,modeArgs = ([], Just commandargsflag)
 ,modeGroupFlags = Group {
     groupUnnamed = []
    ,groupHidden = []
    ,groupNamed = [(generalflagstitle, generalflags1)]
    }
 }

transactionsmode = (commandmode ["transactions"]) {
  modeHelp = "show matched transactions and balance in some account(s)"
 ,modeArgs = ([], Just commandargsflag)
 ,modeGroupFlags = Group {
     groupUnnamed = []
    ,groupHidden = []
    ,groupNamed = [(generalflagstitle, generalflags1)]
    }
 }

activitymode = (commandmode ["activity","histogram"]) {
  modeHelp = "show a barchart of transactions per interval"
 ,modeHelpSuffix = ["The default interval is daily."]
 ,modeArgs = ([], Just commandargsflag)
 ,modeGroupFlags = Group {
     groupUnnamed = []
    ,groupHidden = []
    ,groupNamed = [(generalflagstitle, generalflags1)]
    }
 }

statsmode = (commandmode ["stats"]) {
  modeHelp = "show quick statistics for a journal (or part of it)"
 ,modeArgs = ([], Just commandargsflag)
 ,modeGroupFlags = Group {
     groupUnnamed = []
    ,groupHidden = []
    ,groupNamed = [(generalflagstitle, generalflags1)]
    }
 }

-- 2. ADT holding options used in this package and above, parsed from RawOpts.
-- This represents the command-line options that were provided, with all
-- parsing completed, but before adding defaults or derived values (XXX add)

-- cli options, used in hledger and above
data CliOpts = CliOpts {
     rawopts_         :: RawOpts
    ,command_         :: String
    ,file_            :: Maybe FilePath
    ,alias_           :: [String]
    ,debug_           :: Bool
    ,no_new_accounts_ :: Bool           -- add
    ,rules_file_      :: Maybe FilePath -- convert
    ,reportopts_      :: ReportOpts
 } deriving (Show)

defcliopts = CliOpts
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
toCliOpts :: RawOpts -> IO CliOpts
toCliOpts rawopts = do
  d <- getCurrentDay
  return defcliopts {
              rawopts_         = rawopts
             ,command_         = stringopt "command" rawopts
             ,file_            = maybestringopt "file" rawopts
             ,alias_           = listofstringopt "alias" rawopts
             ,debug_           = boolopt "debug" rawopts
             ,no_new_accounts_ = boolopt "no-new-accounts" rawopts -- add
             ,rules_file_      = maybestringopt "rules-file" rawopts -- convert
             ,reportopts_ = defreportopts {
                             begin_     = maybesmartdateopt d "begin" rawopts
                            ,end_       = maybesmartdateopt d "end" rawopts
                            ,period_    = maybeperiodopt d rawopts
                            ,cleared_   = boolopt "cleared" rawopts
                            ,uncleared_ = boolopt "uncleared" rawopts
                            ,cost_      = boolopt "cost" rawopts
                            ,depth_     = maybeintopt "depth" rawopts
                            ,display_   = maybedisplayopt d rawopts
                            ,effective_ = boolopt "effective" rawopts
                            ,empty_     = boolopt "empty" rawopts
                            ,no_elide_  = boolopt "no-elide" rawopts
                            ,real_      = boolopt "real" rawopts
                            ,flat_      = boolopt "flat" rawopts -- balance
                            ,drop_      = intopt "drop" rawopts -- balance
                            ,no_total_  = boolopt "no-total" rawopts -- balance
                            ,daily_     = boolopt "daily" rawopts
                            ,weekly_    = boolopt "weekly" rawopts
                            ,monthly_   = boolopt "monthly" rawopts
                            ,quarterly_ = boolopt "quarterly" rawopts
                            ,yearly_    = boolopt "yearly" rawopts
                            ,format_    = maybestringopt "format" rawopts
                            ,patterns_  = words'' prefixes $ singleQuoteIfNeeded $ stringopt "args" rawopts
                            }
             }

-- | Get all command-line options, specifying any extra commands that are allowed, or fail on parse errors.
getHledgerCliOpts :: [String] -> IO CliOpts
getHledgerCliOpts addons = do
  args <- getArgs
  toCliOpts (decodeRawOpts $ processValue (mainmode addons) $ tempMoveFlagsAfterCommand args) >>= checkCliOpts

-- utils

-- | Get the unique suffixes (without hledger-) of hledger-* executables
-- found in the current user's PATH, or the empty list if there is any
-- problem.
getHledgerAddonCommands :: IO [String]
getHledgerAddonCommands = map (drop (length progname + 1)) `fmap` getHledgerProgramsInPath

-- | Get the unique names of hledger-* executables found in the current
-- user's PATH, or the empty list if there is any problem.
getHledgerProgramsInPath :: IO [String]
getHledgerProgramsInPath = do
  pathdirs <- splitOn ":" `fmap` getEnvSafe "PATH"
  pathexes <- concat `fmap` mapM getDirectoryContentsSafe pathdirs
  return $ nub $ sort $ filter (isRight . parsewith hledgerprog) pathexes
    where
      hledgerprog = string progname >> char '-' >> many1 (letter <|> char '-') >> eof

getEnvSafe v = getEnv v `catch` (\_ -> return "")
getDirectoryContentsSafe d = getDirectoryContents d `catch` (\_ -> return [])

-- | Convert possibly encoded option values to regular unicode strings.
decodeRawOpts = map (\(name,val) -> (name, fromPlatformString val))

-- workaround for http://code.google.com/p/ndmitchell/issues/detail?id=457
-- just handles commonest case, -f option before command
tempMoveFlagsAfterCommand (fflagandval@('-':'f':_:_):cmd:rest) = cmd:fflagandval:rest
tempMoveFlagsAfterCommand ("-f":fval:cmd:rest) = cmd:"-f":fval:rest
tempMoveFlagsAfterCommand as = as

optserror = error' . (++ " (run with --help for usage)")

setopt name val = (++ [(name,singleQuoteIfNeeded val)])

setboolopt name = (++ [(name,"")])

in_ :: String -> RawOpts -> Bool
in_ name = isJust . lookup name

boolopt = in_

maybestringopt name = maybe Nothing (Just . stripquotes) . lookup name

stringopt name = fromMaybe "" . maybestringopt name

listofstringopt name rawopts = [stripquotes v | (n,v) <- rawopts, n==name]

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

maybedisplayopt :: Day -> RawOpts -> Maybe DisplayExpr
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

-- | Do final validation of processed opts, raising an error if there is trouble.
checkCliOpts :: CliOpts -> IO CliOpts -- or pure..
checkCliOpts opts@CliOpts{reportopts_=ropts} = do
  case formatFromOpts ropts of
    Left err -> optserror $ "could not parse format option: "++err
    Right _ -> return ()
  return opts

-- | Parse any format option provided, possibly raising an error, or get
-- the default value.
formatFromOpts :: ReportOpts -> Either String [FormatString]
formatFromOpts = maybe (Right defaultBalanceFormatString) parseFormatString . format_

-- | Default line format for balance report: "%20(total)  %2(depth_spacer)%-(account)"
defaultBalanceFormatString :: [FormatString]
defaultBalanceFormatString = [
      FormatField False (Just 20) Nothing Total
    , FormatLiteral "  "
    , FormatField True (Just 2) Nothing DepthSpacer
    , FormatField True Nothing Nothing Format.Account
    ]

-- | Get the journal file path from options, an environment variable, or a default
journalFilePathFromOpts :: CliOpts -> IO String
journalFilePathFromOpts opts = do
  f <- myJournalPath
  return $ fromMaybe f $ file_ opts

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

printModeHelpAndExit mode = putStr (showModeHelp mode) >> exitSuccess

showModeHelp = showText defaultWrap . helpText HelpFormatDefault

printVersionAndExit = putStrLn progversion >> exitSuccess

tests_Hledger_Cli_Options = TestList
 [
 ]
