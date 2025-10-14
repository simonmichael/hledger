{-|

Check and show the status of the hledger installation.

-}
{-
Old design notes:

## Goals
- Make getting started with hledger and PTA easier and quicker, to
  (a) make this practical for more people (non techies, busy techies..)
  (b) free up more time and energy for learning and doing PTA and finance

Subgoals:
- Reduce the special knowledge and docs needed after install, to get a standard setup working (especially on windows)
- Help with discovery and setup of advanced quality of life terminal features
- Assist with setting up a first or new journal file
- Deliver relevant install/setup/config expertise more efficiently, freeing up dev and support time

## Design
Why a built in command and not a shell script, haskell script, or docs ?
So it is available and runs reliably anywhere a hledger executable runs, and so it can detect more context-specific advice.
(Optionally use shell or haskell scripts for prototyping, if that's helpful)

## Setup checks
Somewhat ordered.
These deal with the complexities of terminals, the shell, GHC, hledger, and the user's machine, locale, and data.
Initially just informational hints.
Later, add automated or interactive diagnosis and repair attempts,
and semi-persistent state (invalidated at suitable times).
These checks are a necessary evil/stopgap; long term, automate/replace/remove them whenever possible.

-}

-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# OPTIONS_GHC -Wno-unused-matches #-}

module Hledger.Cli.Commands.Setup (
  setupmode
 ,setup
)
where

import Control.Exception
import Control.Monad
-- import Data.ByteString qualified as B
import Data.Char
import Data.Default (def)
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Client
import Network.HTTP.Types (statusCode, hLocation)
import Network.HTTP.Req as R
import Safe
import System.Directory
import System.Environment (lookupEnv)
import System.Exit
import System.FilePath
import System.Info
import System.Process
import Text.Printf (printf)

import Hledger hiding (setupPager)
import Hledger.Cli.CliOptions
import Hledger.Cli.Conf
import Hledger.Cli.Version
import System.IO (localeEncoding, hFlush, stdout)
import Data.Either (isLeft, isRight)


setupmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Setup.txt")
  []
  [generalflagsgroup3]
  []
  ([], Nothing)


{- | Test and print the status of various aspects of the hledger installation.
May also show extra info and hints on how to fix problems.
The goal is to detect and show as much useful information as possible,
and to complete this task reliably regardless of what we find,
without premature termination or misformatting.

The tests are grouped into setup* routines, so named because they might do more
than just test in future.

This is the second version of setup. If it finds that the currently
running hledger is not the one installed in PATH (by comparing --version output),
it refuses to proceed further until that has been done.
This means it can rely on all the latest features and use the hledger API
within this process, simplifying things greatly.
-}
setup :: CliOpts -> Journal -> IO ()
setup _opts@CliOpts{rawopts_=_rawopts, reportspec_=_rspec} _ignoredj = do
  -- This command is not given a journal and should not use _ignoredj;
  -- instead read it ourselves when we are ready.
  putStrLn "Checking your hledger setup.."
  color <- useColorOnStdout
  when color $ 
    putStrLn $ "Legend: " <> intercalate ", " [
       ansiGood    "good"
      ,ansiNeutral "neutral"
      ,ansiWarning "unknown"
      ,ansiBad     "warning"
      ]
  meconf <- setupHledger
  setupTerminal meconf
  setupJournal meconf
  putStr "\n"

------------------------------------------------------------------------------

-- Returns Nothing if no config file was found,
-- or Just the read error or config if it was found.
setupHledger :: IO (Maybe (Either String Conf))
setupHledger = do
  pgroup "hledger"

  -- Output helpers like pbool and pcase make this code more convoluted, but
  -- they provide a valuable testing aid: with --debug they show all possible outputs.

  pbool "is a released version ?"
    (isReleaseVersion $ hbinPackageVersion binaryinfo)
    (p Y prognameandversion)
    (i N prognameandversion)

  elatestversionnumstr <- getLatestHledgerVersion
  let
    mlatestversion = either (const Nothing) toVersion elatestversionnumstr
    latestVersionStr = either (const "") id elatestversionnumstr
    msg = showVersion (hbinPackageVersion binaryinfo) <> " installed, latest is " <> latestVersionStr
  -- flush this output to show what's happening in case of delay or a network access warning
  pcaseFlush "is up to date ? checking..."
    [ (isLeft elatestversionnumstr,
       p U ("couldn't read " <> latestHledgerVersionUrlStr <> " , " <> either id (const "") elatestversionnumstr))
    , (isNothing mlatestversion,
       p U "couldn't parse latest version number")
    , (hbinPackageVersion binaryinfo >= fromJust mlatestversion,
       p Y msg)
    , (hbinPackageVersion binaryinfo < fromJust mlatestversion,
       p N msg)
    ]

  let mArch = hbinArch binaryinfo
  pcase "is a native binary for this machine ?"
    [ (isNothing mArch,
       p U "couldn't detect this binary's architecture")
    , (isJust mArch && fromJust mArch /= arch,
       p N $ "binary is for " <> fromJust mArch <> ", system is " <> arch <> ", may run slowly")
    , (isJust mArch && fromJust mArch == arch,
       p Y (fromJust mArch))
    ]

  -- pdesc "is installed in PATH (this version) ?"
  -- pathexes  <- findExecutables progname
  -- let
  --   (failaction, failmsg) =
  --     -- (exitFailure , "Please install this hledger in PATH then run setup again.")
  --     (return ()   , " Some of this info may not apply to that hledger version. Continuing anyway..")
  -- case pathexes of
  --   [] -> p N failmsg >> failaction
  --   exe:_ -> do
  --     eerrout <- tryHledgerArgs [["--version", "--no-conf"], ["--version"]]
  --     case eerrout of
  --       Left  err -> p U (progname <> " --version failed: " <> err) >> failaction
  --       Right out -> do
  --         case parseHledgerVersion out of
  --           Left  _ -> p U ("couldn't parse " <> progname <> " --version: " <> rstrip out) >> exitFailure
  --           Right pathbin -> do
  --             let pathversion = hbinVersionOutput pathbin
  --             if pathversion /= prognameandversion
  --             then p N (chomp $ unlines [
  --                ""
  --               ," A different hledger version was found in PATH: " <> pathversion
  --               ," at: " <> exe
  --               ,failmsg
  --               ]) >> failaction
  --             else p Y exe
  pathexes  <- findExecutables progname
  let
    exe = headDef "" pathexes
    (failaction, failmsg) =
      -- (exitFailure , "Please install this hledger in PATH then run setup again.")
      (return ()   , " Some of this info may not apply to that hledger version. Continuing anyway..")
  -- Check for a hledger in PATH in various ways, getting a single PathCheckResult
  result <- if null pathexes
    then return PCNotFound
    else do
      everout <- tryHledgerArgs [["--version", "--no-conf"], ["--version"]]
      return $ case everout of
        Left err -> PCVersionFailed err
        Right out -> case parseHledgerVersion out of
          Left _ -> PCVersionUnparseable out
          Right pathbin ->
            let pathversion = hbinVersionOutput pathbin
            in if pathversion == prognameandversion
               then PCVersionMatch
               else PCVersionMismatch pathversion
  -- Print the appropriate test result, or all of them when in debug mode
  pcase "is installed in PATH (this version) ?"
    [ (case result of PCNotFound -> True; _ -> False,
       p N failmsg >> failaction)
    , (case result of PCVersionFailed _ -> True; _ -> False,
       p U (progname <> " --version failed: " <> case result of PCVersionFailed err -> err; _ -> "") >> failaction)
    , (case result of PCVersionUnparseable _ -> True; _ -> False,
       p U ("couldn't parse " <> progname <> " --version: " <> rstrip (case result of PCVersionUnparseable out -> out; _ -> "")))
    , (case result of PCVersionMismatch _ -> True; _ -> False,
       p N (chomp $ unlines [
          ""
         ," A different hledger version was found in PATH: " <> case result of PCVersionMismatch ver -> ver; _ -> ""
         ," at: " <> exe
         ,failmsg
         ]) >> failaction)
    , (case result of PCVersionMatch -> True; _ -> False,
       p Y exe)
    ]

  let encoding = localeEncoding  -- the initial system encoding
  pbool "has a system text encoding configured ?"
    (map toLower (show encoding) /= "ascii")
    (p Y (show encoding <> ", data files must use this encoding"))
    (p N (show encoding <> ", please configure an encoding for non-ascii data"))

  -- pdesc "can handle UTF-8 text ?"
  -- let
  --   eAcuteUtf8   = B.pack [0xC3, 0xA9]
  --   eAcuteLatin1 = B.pack [0xE9]
  -- case T.decodeUtf8' eAcuteUtf8 of
  --   Left _  -> p N "hledger's docs and examples use UTF-8"
  --   Right t -> p Y (T.unpack t)

  -- pdesc "can report text decoding failures ?"
  -- i U (T.unpack $ T.decodeUtf8 eAcuteLatin1)

  muf <- activeUserConfFile
  mlf <- activeLocalConfFile
  let err = error' "setup: unexpected empty muf value"  -- PARTIAL: shouldn't happen, because of pbool's logic
  pbool "has a user config file ?"
    (isJust muf)
    (i Y (fromMaybe err muf <> if isJust mlf then " (overridden)" else ""))
    (i N "")

  let err = error' "setup: unexpected empty mlf value"  -- PARTIAL: shouldn't happen, because of pbool's logic
  pbool "has a local config file ?"
    (isJust mlf)
    (i Y (fromMaybe err mlf))
    (i N "")

  if (isJust muf || isJust mlf) then do
    econf <- getConf def
    pbool "the config file is readable ?"
      (isRight econf)
      (p Y "")
      (p N $ either id (const "") econf)
    case econf of
      Left e          -> return $ Just (Left e)
      Right (conf, _) -> return $ Just (Right conf)
  else
    return Nothing

-- | Various possible results of checking for hledger in PATH.
data PathCheckResult = PCNotFound | PCVersionFailed String | PCVersionUnparseable String | PCVersionMismatch String | PCVersionMatch

------------------------------------------------------------------------------

setupTerminal meconf = do
  pgroup "terminal"
  let
    -- Find the last opt/arg matched by a predicate in the general config, if there is one.
    conflookup predicate = case meconf of
      Just (Right conf) -> find predicate $ reverse $ confLookup "general" conf
      _ -> Nothing

  pdesc "the NO_COLOR variable is defined ?"
  mnocolor <- lookupEnv "NO_COLOR"
  case mnocolor of
    Nothing -> i N ""
    Just _  -> i Y ""

  meconfigcolor <- do
    pdesc "--color is configured by config file ?"
    let mcolorarg = conflookup (\a -> any (`isPrefixOf` a) ["--color", "--colour"])
    case mcolorarg of
      Nothing -> i N "" >> return Nothing
      Just a  -> do
        i Y a
        let
          arg = reverse $ takeWhile (`notElem` ['=',' ']) $ reverse a
        return $ Just $ parseYNA arg

  pdesc "hledger will use color by default ?"
  case (meconfigcolor, isJust mnocolor) of
    (Just (Right Yes), _)     -> p Y ""
    (Just (Right No),  _)     -> i N ""
    (_,                True)  -> i N ""
    (_,                False) -> p Y ""

  pdesc "the PAGER variable is defined ?"
  mv <- lookupEnv "PAGER"
  case mv of
    Nothing -> i N ""
    Just v  -> i Y v

  pdesc "--pager is configured by config file ?"
  let mpagerarg = conflookup ("--pager" `isPrefixOf`)
  meconfpager <- case mpagerarg of
    Nothing -> i N "" >> return Nothing
    Just a  -> do
      i Y a
      let arg = reverse $ takeWhile (`notElem` ['=',' ']) $ reverse a
      return $ Just $ parseYNA arg

  pdesc "hledger will use a pager when needed ?"
  mpager <- findPager
  case mpager of
    Nothing    -> i N "no pager was found"
    Just pager ->
      case meconfpager of
        Just (Right No) -> i N "disabled in config file"
        _ -> do
          p Y pager

          when (map toLower (takeBaseName pager) == "more") $ do
            pdesc "the MORE variable is defined ?"
            mv <- lookupEnv "MORE"
            case mv of
              Nothing -> i N ""
              Just v  -> i Y v

          when (map toLower (takeBaseName pager) == "less") $ do
            pdesc "the LESS variable is defined ?"
            mLESS <- lookupEnv "LESS"
            case mLESS of
              Nothing -> i N ""
              Just _  -> i Y ""

            pdesc "the HLEDGER_LESS variable is defined ?"
            mHLEDGER_LESS <- lookupEnv "HLEDGER_LESS"
            case mHLEDGER_LESS of
              Nothing -> i N ""
              Just v  -> i Y v

            when (isNothing mHLEDGER_LESS) $ do
              pdesc "adjusting LESS variable for color etc. ?"
              usecolor <- useColorOnStdout
              i (if usecolor then Y else N) ""

  pdesc "tables will use box-drawing chars ?"
  if isJust $ conflookup ("--pretty"==)
  then p Y ""
  else i N "you can use --pretty to enable them"

  -- pdesc "bash shell completions are installed ?" >> p U ""
  -- pdesc "zsh shell completions are installed ?" >> p U ""

------------------------------------------------------------------------------

setupJournal meconf = do
  pgroup "journal"
  let
    -- Find the last opt/arg matched by a predicate in the general config, if there is one.
    conflookup predicate = case meconf of
      Just (Right conf) -> find predicate $ reverse $ confLookup "general" conf
      _ -> Nothing

  -- pdesc "a home directory journal file exists ?"
  -- mh <- getHomeSafe
  -- (ok,msg) <- case mh of
  --   Just h -> do
  --     let f = h </> journalDefaultFilename
  --     e <- doesFileExist f
  --     return (if e then Y else N, if e then f else "")
  --   Nothing -> return (N, "")
  -- i ok msg

  pdesc "the LEDGER_FILE variable is defined ?"
  mf <- lookupEnv journalEnvVar
  let
    (ok, msg) = case mf of
      Just f  -> (Y, f)
      Nothing -> (N, "")
  i ok msg

  -- case mf of
  --   Nothing -> return ()
  --   Just f -> do
  --     pdesc "$LEDGER_FILE journal exists ?"
  --     e <- doesFileExist f
  --     i e "" ""

  -- when (isJust mh && isJust mf) $ do
  --   pdesc "$LEDGER_FILE is masking home journal ?"
  --   i Y ""

  pdesc "a default journal file is readable ?"
  jfile <- defaultJournalPath
  -- let
  --   args = concat [
  --     ["print"],
  --     ["--ignore-assertions" | supportsIgnoreAssertions version],
  --     ["--no-conf" | supportsConfigFiles version]
  --     ]
  -- (exit, _, err) <- readProcessWithExitCode progname args ""
  -- XXX can this ignore assertions and config files, like the above ?
  ej <- defaultJournalSafely
  case ej of
    Left estr -> p N (jfile <> ":\n" <> estr)
    Right j@Journal{..} -> do
      p Y jfile

      pdesc "it includes additional files ?"
      let numfiles = length jfiles
      if numfiles > 1
      then i Y (show (numfiles - 1) <> " files")
      else i N ""

      pdesc "all commodities are declared ?"
      let
        numcommodities = length $ journalCommodities j
        undeclaredcommodities = journalCommoditiesUsed j \\ journalCommoditiesDeclared j
      if null undeclaredcommodities
      then p Y (show numcommodities <> " commodities")
      else w N (show (length undeclaredcommodities) <> " undeclared commodities")

      let
        accttypes = [Asset, Liability, Equity, Revenue, Expense, Cash, Conversion]
        typesdeclaredorinferred = nub $ M.elems jaccounttypes
        typesnotfound = filter (not.(`elem` typesdeclaredorinferred)) accttypes
        acctswithdeclaredorinferredtype = nub (M.keys jaccounttypes)
        numaccts = length $ journalAccountNames j
        untypedaccts = journalAccountNames j \\ acctswithdeclaredorinferredtype
        undeclaredaccts = journalAccountNamesUsed j \\ journalAccountNamesDeclared j
        -- hasdeclaredaccts t = case M.lookup t jdeclaredaccounttypes of
        --   Just (_ : _) -> True
        --   _ -> False

      -- pdesc "Asset accounts declared ?"    
      -- if hasdeclaredaccts Asset then i Y "" else i N ""

      -- pdesc "Liability accounts declared ?"
      -- if hasdeclaredaccts Liability then i Y "" else i N ""

      -- pdesc "Equity accounts declared ?"
      -- if hasdeclaredaccts Equity then i Y "" else i N ""

      -- pdesc "Revenue accounts declared ?"
      -- if hasdeclaredaccts Revenue then i Y "" else i N ""

      -- pdesc "Expense accounts declared ?"
      -- if hasdeclaredaccts Expense then i Y "" else i N ""

      -- pdesc "Cash accounts declared ?"
      -- if hasdeclaredaccts Cash then i Y "" else i N ""

      -- pdesc "Conversion accounts declared ?"
      -- if hasdeclaredaccts Conversion then i Y "" else i N ""  -- ("--infer-equity will use a default conversion account name")

      -- XXX hard to detect accounts where type was inferred from name
      -- unless arealltypesdeclared $ do
      -- let
      --   acctswithdeclaredtype           = concat (M.elems jdeclaredaccounttypes)
      --   acctswithinferredtype           = acctswithdeclaredorinferredtype \\ acctswithdeclaredtype
      --   arealltypesdeclared = all hasdeclaredaccts accttypes
      --   typesinferredfromnames =
      --     if arealltypesdeclared then []
      --     else sort $ nub $ catMaybes $ map (flip M.lookup jaccounttypes) acctswithinferredtype
      -- pdesc "types detected from account names ?"
      -- if null typesinferredfromnames then i N "" else i Y (concatMap show typesinferredfromnames)

      pdesc "all accounts are declared ?"
      if null undeclaredaccts
      then p Y (show numaccts <> " accounts")
      else w N (show (length undeclaredaccts) <> " undeclared accounts")

      pdesc "all accounts have types ?"
      if null untypedaccts
      then p Y ""
      else i N (show (length untypedaccts) <> " accounts have no type")

      pdesc "accounts of all types exist ?"
      if null typesnotfound
      then p Y (concatMap show accttypes <> " accounts detected")
      else w N ("no " <> concatMap show typesnotfound <> " accounts found, some features may not work")

      pdesc "commodities/accounts are being checked ?"
      let strict = isJust $ conflookup (\a -> any (==a) ["-s", "--strict"])
      if strict
      then p Y "commodities and accounts must be declared"
      else i N "you can use -s to check them"

      pdesc "balance assertions are being checked ?"
      let ignoreassertions = isJust $ conflookup (\a -> any (==a) ["-I", "--ignore-assertions"])
      if 
        | ignoreassertions && not strict -> i N "you can use -s to check them"
        | not strict                     -> p Y "you can use -I to ignore them"
        | otherwise                      -> p Y "can't ignore assertions (-s in config file)"

------------------------------------------------------------------------------

-- Test a hledger version for support of various features.
ver >=! str = ver >= (fromJust $ toVersion str)
supportsIgnoreAssertions = (>=! "0.24") -- --ignore-assertions (2014)
supportsCommodityDirective = (>=! "1.0") -- commodity directive (2016)
supportsPretty = (>=! "1.2") -- --pretty, to use box-drawing characters (2017)
supportsAccountDirective = (>=! "1.9") -- account directive (2018)
supportsAccountTypes = (>=! "1.13") -- ALERX account types, type: tag (2019)
supportsCashAccountType = (>=! "1.19") -- C/Cash account type (2020)
supportsBasicColor = (>=! "1.19") -- basic color detection/control (2020)
supportsConversionAccountType = (>=! "1.25") -- V/Conversion account type, accounts --types (2022)
supportsConfigFiles = (>=! "1.40") -- config files (2024)
supportsColor = (>=! "1.41") -- robust color detection/control (2024)
supportsPager = (>=! "1.41") -- use a pager for all output (2024)
supportsBashCompletions = (>=! "1.41") -- up to date bash shell completions (2024)

-- Status of a setup question/statement: yes, no, unknown.
data YNU = Y | N | U deriving (Eq)

-- Show a status as unstyled english text.
instance Show YNU where
  show Y = "yes"
  show N = " no"
  show U = "  ?"

-- | Print a status, ANSI-styled and emoji-decorated when permitted, using the good/bad styles for Y/N;
-- and the (possibly empty) provided message. See also 'w' and 'i'.
--
-- Status is communicated to the user
-- 1. as text: "yes"/"no"/"?"
--
-- and when colour is permitted,
-- 2. in one of four ANSI colours
-- 3. with one of four emojis appended, for added distinctiveness in case of colour blindness.
--
-- The emojis chosen are hopefully somewhat likely to render reasonably well even on non-apple machines;
-- and if they don't, 1 and 2 will still carry the message.
--
-- Note these things are distinct and not necessarily corresponding, which could be confusing:
-- - "good"/"neutral"/"warning"/"bad" test status, in display text & user's perspective
-- - ansiGood/ansiNeutral/ansiWarning/ansiBad styles, defined below
-- - warn[IO] and error functions defined elsewhere, which apply their own warning and error ANSI styles,
--   to (possibly ANSI-styled) text.
--
p :: YNU -> String -> IO ()
p status msg = putStrLn $ unwords ["", showGoodBad status, "", msg]
  where
    showGoodBad Y = ansiGood    $ "yes" `andIfColour` checkmarkInGreenBoxEmoji
    showGoodBad N = ansiBad     $ " no" `andIfColour` redExclamationMarkEmoji
    showGoodBad U = ansiWarning $ "  ?" `andIfColour` yellowDiamondEmoji

-- | Print a status, ANSI-styled and emoji-decorated when permitted, using the good/warning styles for Y/N;
-- and the (possibly empty) provided message.
w :: YNU -> String -> IO ()
w status msg = putStrLn $ unwords ["", showGoodWarn status, "", msg]
  where
    showGoodWarn Y = ansiGood    $ "yes" `andIfColour` iInBlueBoxEmoji
    showGoodWarn N = ansiWarning $ " no" `andIfColour` yellowDiamondEmoji
    showGoodWarn U = ansiWarning $ "  ?" `andIfColour` yellowDiamondEmoji

-- | Print a status, ANSI-styled and emoji-decorated when permitted, using the neutral style for Y/N;
-- and the (possibly empty) provided message.
i :: YNU -> String -> IO ()
i status msg = putStrLn $ unwords ["", showNeutral status, "", msg]
  where
    showNeutral Y = ansiNeutral $ "yes" `andIfColour` iInBlueBoxEmoji
    showNeutral N = ansiNeutral $ " no" `andIfColour` iInBlueBoxEmoji
    showNeutral U = ansiWarning $ "  ?" `andIfColour` yellowDiamondEmoji

-- Apply setup-status-related ANSI styles to text.
ansiGood    = bold' . brightGreen'
ansiNeutral = bold' . brightBlue'
ansiWarning = bold' . brightYellow'
ansiBad     = bold' . brightRed'

-- Use only reasonably well-supported emojis here.
checkmarkInGreenBoxEmoji = "✅"
redExclamationMarkEmoji  = "❗"
yellowDiamondEmoji       = "🔸"
largeYellowDiamondEmoji  = "🔶"
-- This one may render as monochrome in some terminals ?
-- Also it seems more likely to be single rather than double width, so a space is added to compensate.
iInBlueBoxEmoji          = "ℹ️ "

-- Append a space and the second text, if colour is permitted on stdout (using 'useColorOnStdoutUnsafe').
andIfColour a b = a <> if useColorOnStdoutUnsafe then " " <> b else ""

-- | Print a setup test group's heading.
pgroup :: String -> IO ()
pgroup s = putStrLn $ "\n" <> bold' s

-- | Print a setup test's description, formatting and padding it to a fixed width.
pdesc :: String -> IO ()
pdesc s = printf "* %-40s" s

-- Print the output for a test, formatting the given description,
-- running the appropriate output action based on the given (lazy) boolean test result.
-- Or with --debug, print the outputs for both cases (useful for output testing).
pbool :: String -> Bool -> IO () -> IO () -> IO ()
pbool desc result showtrue showfalse
  | debugLevel > 0 = t >> f
  | result = t
  | otherwise = f
 where
  t = pdesc desc >> showtrue
  f = pdesc desc >> showfalse

-- Like pbool, but flush output after printing the test's description,
-- to help show what's going on if evaluating the test result takes a long time
-- or pops up a network access warning.
pboolFlush :: String -> Bool -> IO () -> IO () -> IO ()
pboolFlush name result showtrue showfalse
  | debugLevel > 0 = t >> f
  | result = t
  | otherwise = f
 where
  t = pdesc name >> hFlush stdout >> showtrue
  f = pdesc name >> hFlush stdout >> showfalse

-- | A generalisation of pbool, that takes a list of (matched, action) pairs.
-- Runs the first action where matched was true.
-- Or with --debug, runs all actions in sequence, printing all possible test outputs.
pcase :: String -> [(Bool, IO ())] -> IO ()
pcase name cases
  | debugLevel > 0 = mapM_ ((pdesc name >>) . snd) cases
  | otherwise = pdesc name >> fromMaybe (return ()) (lookup True cases)

-- | Like pboolFlush, but for pcase.
pcaseFlush :: String -> [(Bool, IO ())] -> IO ()
pcaseFlush name cases
  | debugLevel > 0 = mapM_ ((pdesc name >> hFlush stdout >>) . snd) cases
  | otherwise = pdesc name >> hFlush stdout >> fromMaybe (return ()) (lookup True cases)

-- | Like pcase, but takes a test result value and a list of (value, action) pairs.
-- Runs the first action where the value matches the result value.
-- Or with --debug, runs all actions in sequence, printing all possible test outputs.
-- pmatch :: (Eq a) => String -> a -> [(a, IO ())] -> IO ()
-- pmatch name result cases
--   | debugLevel > 0 = mapM_ ((pdesc name >>) . snd) cases
--   | otherwise = pdesc name >> fromMaybe (return ()) (lookup result cases)

(getLatestHledgerVersion, latestHledgerVersionUrlStr) =
  -- (getLatestHledgerVersionFromHackage, "https://hackage.haskell.org/package/hledger/docs")
  (getLatestHledgerVersionFromHledgerOrg, "https://hledger.org/install.html")

httptimeout = 10000000  -- 10s

-- | Get the current hledger release version from the internet.
-- Currently requests the latest doc page from Hackage and inspects the redirect path.
-- Should catch all normal errors, and time out after 10 seconds.
getLatestHledgerVersionFromHackage :: IO (Either String String)
getLatestHledgerVersionFromHackage = do
  let url = https "hackage.haskell.org" /: "package" /: "hledger" /: "docs" /: ""
  result <- try $ runReq defaultHttpConfig{httpConfigRedirectCount=0} $
    req HEAD url NoReqBody bsResponse (R.responseTimeout httptimeout)
  case result of
    Right _ -> return $ Left "expected a redirect"
    Left (VanillaHttpException (HttpExceptionRequest _ (StatusCodeException rsp _))) -> do
      let status = statusCode $ responseStatus rsp
      if status >= 300 && status < 400
        then do
          let locationHeader = lookup hLocation (responseHeaders rsp)
          case fmap T.decodeUtf8 locationHeader of
            Nothing       -> return $ Left "no Location header"
            Just location -> do
              let packagename = take 1 $ drop 1 $ reverse $ T.splitOn "/" location
              case packagename of
                [n] -> return $ Right $ dropWhile (`notElem` ['0'..'9']) $ T.unpack n
                _   -> return $ Left "couldn't parse Location"
        else return $ Left $ "HTTP status " ++ show status
    Left err -> return $ Left $ "other exception: " ++ show err

-- | Like the above, but get the version from the first number on the hledger.org Install page.
getLatestHledgerVersionFromHledgerOrg :: IO (Either String String)
getLatestHledgerVersionFromHledgerOrg = do
  let url = https "hledger.org" /: "install.html"
  do
    result <- try $ runReq defaultHttpConfig $ req GET url NoReqBody bsResponse (R.responseTimeout httptimeout)
    case result of
      Left (e :: R.HttpException) -> return $ Left $ show e
      Right rsp -> case T.decodeUtf8' $ R.responseBody rsp of
        Left e  -> return $ Left $ show e
        Right t -> return $
          if null version then Left "couldn't parse version" else Right version
          where
            -- keep synced
            versionline = take 1 $ dropWhile (not . ("current hledger release" `isInfixOf`)) $ lines $ T.unpack t
            version = takeWhile (`elem` ("0123456789."::[Char])) $ dropWhile (not . isDigit) $ headDef "" $ versionline
  -- work around potential failure on mac (& possible security issue, reported upstream)
  `catch` (\(_ :: IOError) -> return $ Left "req failed (mac PATH issue ?)")

-- | Try to run the hledger in PATH with one or more sets of command line arguments.
-- Returns the output from the first set of arguments that runs successfully,
-- or the error output from the last set.
tryHledgerArgs :: [[String]] -> IO (Either String String)
tryHledgerArgs [] = pure $ Left "tryHledgerArgs: no arguments provided"
tryHledgerArgs (args:rest) = do
  eresult <- runHledger args
  case eresult of
    Right out -> pure $ Right out
    Left err -> if null rest then pure $ Left err else tryHledgerArgs rest

-- | Run the hledger in PATH with the given command line arguments,
-- returning the output on success or the error output on failure.
runHledger :: [String] -> IO (Either String String)
runHledger args = do
  (exit, out, err) <- readProcessWithExitCode "hledger" args ""
  pure $ case exit of
    ExitSuccess -> Right out
    ExitFailure _ -> Left err
