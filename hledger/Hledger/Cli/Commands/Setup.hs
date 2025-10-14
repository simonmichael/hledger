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
import Network.HTTP.Types (statusCode)  --, hLocation)
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

  -- The pbool and pcase output helpers sometimes make this code convoluted,
  -- but they provide a valuable aid for testing this command:
  -- with --debug they show all possible outputs.

  pbool "is a released version ?"
    (isReleaseVersion $ hbinPackageVersion binaryinfo)
    (p Y prognameandversion)
    (i N prognameandversion)

  elatestversionnumstr <- getLatestHledgerVersion
  let
    mlatestversion = either (const Nothing) toVersion elatestversionnumstr
    latestVersionStr = either (const "") id elatestversionnumstr
    msg = showVersion (hbinPackageVersion binaryinfo) <> " installed, latest is " <> latestVersionStr
  -- use pcaseFlush to show this output before the HTTP request, to show what's happening in case of delay or a network access warning
  -- XXX ineffective, getLatestHledgerVersion runs first
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

  muf <- activeUserConfFile
  mlf <- activeLocalConfFile
  pbool "has a user config file ?"
    (isJust muf)
    (i Y (maybe "" id muf <> if isJust mlf then " (overridden)" else ""))
    (i N "")

  pbool "has a local config file ?"
    (isJust mlf)
    (i Y (maybe "" id mlf))
    (i N "")

  if (isJust muf || isJust mlf) then do
    econf <- getConf def
    pbool "the config file is readable ?"
      (isRight econf)
      (p Y "")
      (p N $ either ('\n':) (const "actually it's fine") econf)
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

  mnocolor <- lookupEnv "NO_COLOR"
  pbool "the NO_COLOR variable is defined ?"
    (isJust mnocolor)
    (i Y "")
    (i N "")

  let mconfigcoloropt = conflookup (\a -> any (`isPrefixOf` a) ["--color", "--colour"])
  pbool "--color is configured by config file ?"
    (isJust mconfigcoloropt)
    (i Y (maybe "" id mconfigcoloropt))
    (i N "")

  let mconfigcolor = mconfigcoloropt >>= either (const Nothing) Just . parseYNA . optval
        where optval = reverse . takeWhile (`notElem` ['=',' ']) . reverse
  pcase "hledger will use color by default ?"
    [ (mconfigcolor == Just Yes, p Y "")
    , (mconfigcolor == Just No,  i N "")
    , (isJust mnocolor,          i N "")
    , (True,                     p Y "")  -- default
    ]

  mv <- lookupEnv "PAGER"
  pbool "the PAGER variable is defined ?"
    (isJust mv)
    (i Y (maybe "" id mv))
    (i N "")

  let mpagerarg = conflookup ("--pager" `isPrefixOf`)
  pbool "--pager is configured by config file ?"
    (isJust mpagerarg)
    (i Y (maybe "" id mpagerarg))
    (i N "")
  let meconfpager = mpagerarg >>= \a ->
        let arg = reverse $ takeWhile (`notElem` ['=',' ']) $ reverse a
        in Just $ parseYNA arg

  mpager <- findPager
  pcase "hledger will use a pager when needed ?"
    [ (isNothing mpager,
       i N "no pager was found")
    , (isJust mpager && meconfpager == Just (Right No),
       i N "disabled in config file")
    , (isJust mpager,
       p Y (maybe "" id mpager))
    ]

  case mpager of
    Nothing -> return ()
    Just _ | meconfpager == Just (Right No) -> return ()
    Just pager -> do

      when (map toLower (takeBaseName pager) == "more") $ do
        mv <- lookupEnv "MORE"
        pbool "the MORE variable is defined ?"
          (isJust mv)
          (i Y (maybe "" id mv))
          (i N "")

      when (map toLower (takeBaseName pager) == "less") $ do
        mLESS <- lookupEnv "LESS"
        pbool "the LESS variable is defined ?"
          (isJust mLESS)
          (i Y "")
          (i N "")

        mHLEDGER_LESS <- lookupEnv "HLEDGER_LESS"
        pbool "the HLEDGER_LESS variable is defined ?"
          (isJust mHLEDGER_LESS)
          (i Y (maybe "" id mHLEDGER_LESS))
          (i N "")

        when (isNothing mHLEDGER_LESS) $ do
          usecolor <- useColorOnStdout
          pbool "adjusting LESS variable for color etc. ?"
            usecolor
            (i Y "")
            (i N "")

  pbool "tables will use box-drawing chars ?"
    (isJust $ conflookup ("--pretty"==))
    (p Y "")
    (i N "you can use --pretty to enable them")

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

  mf <- lookupEnv journalEnvVar
  pbool "the LEDGER_FILE variable is defined ?"
    (isJust mf)
    (i Y (maybe "" id mf))
    (i N "")

  jfile <- defaultJournalPath
  -- XXX can this ignore assertions and config files ?
  ej <- defaultJournalSafely
  pbool "a default journal file is readable ?"
    (isRight ej)
    (p Y jfile)
    (p N $ either ('\n' :) (const "actually it's fine") ej)

  case ej of
    Left _ -> return ()
    Right j@Journal{..} -> do

      let numfiles = length jfiles
      pbool "it includes additional files ?"
        (numfiles > 1)
        (i Y (show (numfiles - 1) <> " files"))
        (i N "")

      let
        numcommodities = length $ journalCommodities j
        undeclaredcommodities = journalCommoditiesUsed j \\ journalCommoditiesDeclared j
      pbool "all commodities are declared ?"
        (null undeclaredcommodities)
        (p Y (show numcommodities <> " commodities"))
        (w N (show (length undeclaredcommodities) <> " undeclared commodities"))

      let
        accttypes = [Asset, Liability, Equity, Revenue, Expense, Cash, Conversion]
        typesdeclaredorinferred = nub $ M.elems jaccounttypes
        typesnotfound = filter (not.(`elem` typesdeclaredorinferred)) accttypes
        acctswithdeclaredorinferredtype = nub (M.keys jaccounttypes)
        numaccts = length $ journalAccountNames j
        untypedaccts = journalAccountNames j \\ acctswithdeclaredorinferredtype
        undeclaredaccts = journalAccountNamesUsed j \\ journalAccountNamesDeclared j

      pbool "all accounts are declared ?"
        (null undeclaredaccts)
        (p Y (show numaccts <> " accounts"))
        (w N (show (length undeclaredaccts) <> " undeclared accounts"))

      pbool "all accounts have types ?"
        (null untypedaccts)
        (p Y "")
        (i N (show (length untypedaccts) <> " accounts have no type"))

      pbool "accounts of all types exist ?"
        (null typesnotfound)
        (p Y (concatMap show accttypes <> " accounts detected"))
        (w N ("no " <> concatMap show typesnotfound <> " accounts found, some features may not work"))

      let strict = isJust $ conflookup (\a -> any (==a) ["-s", "--strict"])
      pbool "commodities/accounts are being checked ?"
        strict
        (p Y "commodities and accounts must be declared")
        (i N "you can use -s to check them")

      let ignoreassertions = isJust $ conflookup (\a -> any (==a) ["-I", "--ignore-assertions"])
      pcase "balance assertions are being checked ?"
        [ (ignoreassertions && not strict, i N "you can use -s to check them")
        , (not strict,                     p Y "you can use -I to ignore them")
        , (otherwise,                      p Y "can't ignore assertions (-s in config file)")
        ]

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

getLatestHledgerVersion    = getLatestHledgerVersionFromHledgerOrg  -- keep
latestHledgerVersionUrlStr = "https://hledger.org/install.html"     -- synced

httptimeout = 10000000  -- 10s

-- | Get the current hledger release version from the first number on the hledger.org/install.html page.
getLatestHledgerVersionFromHledgerOrg :: IO (Either String String)
getLatestHledgerVersionFromHledgerOrg = do
  let url = https "hledger.org" /: "install.html"
  result <- try $ runReq defaultHttpConfig $ req GET url NoReqBody bsResponse (R.responseTimeout httptimeout)
  case result of
    Left e    -> return $ Left $ formatHttpException e
    Right rsp -> case T.decodeUtf8' $ R.responseBody rsp of
      Left e   -> return $ Left $ show e
      Right t  -> return $
        if null version then Left "couldn't parse version" else Right version
        where
          -- keep synced
          versionline = take 1 $ dropWhile (not . ("current hledger release" `isInfixOf`)) $ lines $ T.unpack t
          version = takeWhile (`elem` ("0123456789."::[Char])) $ dropWhile (not . isDigit) $ headDef "" $ versionline
  -- handle a potential req failure on mac (https://github.com/mrkkrp/req/issues/185) more gracefully.
  `catch` (\(_ :: IOError) -> return $ Left "req IO error (if on mac, perhaps PATH is misconfigured ?)")

-- | Convert an HTTP exception to a user-friendly error message.
formatHttpException :: R.HttpException -> String
formatHttpException (VanillaHttpException (HttpExceptionRequest _ err)) = case err of
  -- https://hackage.haskell.org/package/http-client-0.7.17/docs/Network-HTTP-Client.html#t:HttpExceptionContent
  ConnectionTimeout         -> "connection timed out"
  ResponseTimeout           -> "request timed out"
  StatusCodeException rsp _ -> "HTTP status " ++ show (statusCode $ responseStatus rsp)
  ConnectionFailure _       -> "connection failed"   -- these two can be caused
  InternalException _       -> "connection failed"   -- by any kind of exception
  _                         -> "network error: " ++ show err  -- some other failure
formatHttpException (VanillaHttpException (InvalidUrlException _ msg)) = "invalid URL: " ++ msg
formatHttpException (JsonHttpException msg) = "JSON error: " ++ msg

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
