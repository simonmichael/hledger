{-|

Check and show the status of the hledger installation.

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

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
import Data.Default (def)


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

The first group of tests checks the hledger executable found in PATH.
Note, later tests which require running hledger should use this executable,
rather than the API of the hledger currently running (in case they're different).

This hledger executable could be old; we can't assume it has modern flags/commands.
Eg its --version output may be less than usual, or it may not have the check command.

Or it may not accept -n/--no-conf, which we may need to avoid disruption from config files.
Eg there's an inconvenient bug in hledger 1.40-1.42ish: without -n, a bad config file
breaks pretty much everything, including --version, --help, and test.

So we first try hledger --version -n; if this fails, we try hledger --version.
(For absolute precision we should perhaps check the failure output for "Unknown flag: -n"..)
If we are able to detect the version, we use that to decide how and whether to run
later tests. Eg, for hledger <1.40, we won't run config tests, and won't use -n.

-}
setup :: CliOpts -> Journal -> IO ()
setup _opts@CliOpts{rawopts_=_rawopts, reportspec_=_rspec} _ignoredj = do
  -- This command is not given a journal and should not use _ignoredj;
  -- instead detect it ourselves when we are ready.
  putStrLn "Checking your hledger setup.."
  color <- useColorOnStdout
  when color $ 
    putStrLn $ "Legend: " <> intercalate ", " [
       good    "good"
      ,neutral "neutral"
      ,warning "unknown"
      ,bad     "warning"
      ]
  mversion <- setupHledger
  case mversion of
    Nothing -> return ()
    Just HledgerBinaryVersion{hbinPackageVersion=version} -> do
      setupLocale
      conf <- fromMaybe nullconf <$> setupConfig version
      setupColor version conf
      setupPager version conf
      setupPretty version conf
      setupCompletions version
      ej <- setupJournal version
      let mj = either (const Nothing) Just ej
      setupAccounts version mj
      setupCommodities version mj
      return ()
  putStr "\n"

-- Test a hledger version for support of various features.
ver >=! str = ver >= (fromJust $ toVersion str)
supportsIgnoreAssertions      = (>=! "0.24")  -- --ignore-assertions (2014)
supportsCommodityDirective    = (>=! "1.0")   -- commodity directive (2016)
supportsPretty                = (>=! "1.2")   -- --pretty, to use box-drawing characters (2017)
supportsAccountDirective      = (>=! "1.9")   -- account directive (2018)
supportsAccountTypes          = (>=! "1.13")  -- ALERX account types, type: tag (2019)
supportsCashAccountType       = (>=! "1.19")  -- C/Cash account type (2020)
supportsBasicColor            = (>=! "1.19")  -- basic color detection/control (2020)
supportsConversionAccountType = (>=! "1.25")  -- V/Conversion account type, accounts --types (2022)
supportsConfigFiles           = (>=! "1.40")  -- config files (2024)
supportsColor                 = (>=! "1.41")  -- robust color detection/control (2024)
supportsPager                 = (>=! "1.41")  -- use a pager for all output (2024)
supportsBashCompletions       = (>=! "1.41")  -- up to date bash shell completions (2024)

------------------------------------------------------------------------------

-- | This first test group looks for a "hledger" executable in PATH;
-- if found, tests it in various ways;
-- and if it ran successfully, returns the full --version output
-- and the numeric Version parsed from that.
setupHledger :: IO (Maybe HledgerBinaryVersion)
setupHledger = do
  pgroup "hledger"

  pdesc "is in PATH ?"
  pathexes  <- findExecutables progname
  home      <- getHomeDirectory
  appdata   <- getXdgDirectory XdgData ""
  otherexes <- flip findExecutablesInDirectories progname $
    [home </> ".local/bin"
    ,home </> ".cabal/bin"
    ,home </> ".nix-profile/bin"
    ,"/opt/homebrew/bin"
    ,"/usr/local/bin"
    ,"/usr/bin"
    ]
    ++ [appdata </> "local/bin" | os == "mingw32"]
    ++ [appdata </> "cabal/bin" | os == "mingw32"]
  case (pathexes, otherexes) of
    ([], []) -> do
      p N $ "Move the " <> progname <> " binary to a directory in your shell's PATH"
      return Nothing
    ([], otherexe:_) -> do
      let otherdir = takeDirectory otherexe
      p N $ unlines
        ["Add " <> otherdir <> " to PATH in your shell config."
        ,"  Eg on unix: echo 'export PATH=" <> otherdir <> ":$PATH' >> ~/.profile"
        ,"  and start a new shell session."
        ]
      return Nothing
    (pathexe:_, _) -> do
      p Y (quoteIfNeeded pathexe)

      -- hledger was found in PATH, continue

      pdesc "runs and --version looks like hledger ?"
      eerrout <- tryHledgerArgs [["--version", "--no-conf"], ["--version"]]
      case eerrout of
        Left  err ->
          p N (progname <> " --version failed: " <> err) >> return Nothing
        Right out | versionoutput <- rstrip out -> do
          case parseHledgerVersion versionoutput of
            Left  _ -> p N (progname <> " --version shows: " <> rstrip out) >> return Nothing
            Right bininfo@HledgerBinaryVersion{..} -> do
              p Y versionoutput

              -- It runs and --version output looks ok, continue

              pdesc "is a native binary ?"
              case hbinArch of
                Nothing -> p U $ "couldn't detect arch in --version output"
                Just binarch | binarch /= arch -> p N $ "installed binary is for " <> binarch <> ", system is " <> arch
                Just binarch -> p Y binarch

              pdesc "is up to date ?"
              let binversion = hbinPackageVersion
              elatestversionnumstr <- getLatestHledgerVersion
              case elatestversionnumstr of
                Left e -> p U ("couldn't read " <> latestHledgerVersionUrlStr <> " , " <> e)
                Right latestversionnumstr ->
                  case toVersion latestversionnumstr of
                    Nothing -> p U "couldn't parse latest version number"
                    Just latestversion -> p
                      (if binversion >= latestversion then Y else N)
                      (showVersion hbinPackageVersion <> " installed, latest is " <> latestversionnumstr)

              pdesc "is same as the hledger running setup ?"
              if prognameandversion == hbinVersionOutput
              then i Y ""
              else i N prognameandversion

              return $ Just bininfo


------------------------------------------------------------------------------

setupConfig :: Version -> IO (Maybe Conf)
setupConfig version = do
  pgroup "config"

  pdesc "hledger supports config files ?"
  if (not $ supportsConfigFiles version)
  then p N "hledger 1.40+ needed" >> return Nothing
  else do
    p Y ""

    pdesc "a user config file exists ? (optional)"
    muf <- activeUserConfFile
    let
      (ok, msg) = case muf of
        Just f  -> (Y, f)
        Nothing -> (N, "")
    i ok msg

    pdesc "a local config file exists ?"
    mlf <- activeLocalConfFile
    let
      (ok, msg) = case mlf of
        Just f  -> (Y, f) -- <> if isJust muf then " (masking user config)" else "")
        Nothing -> (N, "")
    i ok msg

    when (isJust muf && isJust mlf) $ do
      pdesc "local config is masking user config ?"
      i Y ""

    let mf = mlf <|> muf
    case mf of
      Nothing -> return Nothing
      Just _ -> do
        pdesc "hledger can read the config file ?"
        -- Test config file readability, without requiring journal file readability, forward compatibly.
        (exit, _, err) <- readProcessWithExitCode progname ["print", "-f-"] ""
        case exit of
          ExitFailure _ -> p N ("\n"<>err) >> return Nothing
          ExitSuccess   -> do
            p Y ""

            -- Read config file ourselves for closer inspection
            -- econfsections <- readFile f <&> parseConf f . T.pack
            econf <- getConf def  -- safer; should agree with file detection above
            case econf of
              Left _ -> return Nothing
              Right (conf, _) -> do

                -- pdesc "common general options configured ?"
                -- --pretty --ignore-assertions --infer-costs"
                -- print --explicit --show-costs"

                return $ Just conf

------------------------------------------------------------------------------

setupLocale = do
  pgroup "locale"

  -- pdesc "a locale is set to allow UTF-8 text ?"
  -- pdesc "the locale allows decoding UTF-8 text ?"
  pdesc "the locale allows handling UTF-8 text ?"
  case T.decodeUtf8' $ B.pack [0xC3, 0xA9] of  -- é
    Left _  -> p N "hint: set a UTF-8 locale / enable UTF-8 text"
    Right t -> p Y (T.unpack t)
  --- XXX also recognise non-utf8 locales I guess, as long as some text encoding is supported

------------------------------------------------------------------------------

setupColor version conf = do
  pgroup "color"

  pdesc "hledger supports robust color output ?"
  if not $ supportsColor version then p N "hledger 1.41+ needed"
  else do
    p Y ""

    pdesc "the NO_COLOR variable is defined ?"
    mnocolor <- lookupEnv "NO_COLOR"
    case mnocolor of
      Nothing -> i N ""
      Just _  -> i Y ""

    meconfigcolor <- do
      pdesc "color is configured in the config file ?"  -- we check the general section only
      let
        confgenargs = confLookup "general" conf
        mcolorarg = find (\a -> any (`isPrefixOf` a) ["--color", "--colour"]) confgenargs
      case mcolorarg of
        Nothing -> i N "" >> return Nothing
        Just a  -> do
          i Y a
          let
            arg = reverse $ takeWhile (`notElem` ['=',' ']) $ reverse a
          return $ Just $ parseYNA arg

    pdesc "hledger will use color when possible ?"
    case (meconfigcolor, isJust mnocolor) of
      (Just (Right Yes), _)     -> i Y ""
      (Just (Right No),  _)     -> i N ""
      (_,                True)  -> i N ""
      (_,                False) -> i Y ""

------------------------------------------------------------------------------

setupPager version conf = do
  pgroup "pager"
  
  pdesc "hledger supports paged output ?"
  if not $ supportsPager version then p N "hledger 1.41+ needed"
  else do
    p Y ""

    pdesc "the PAGER variable is defined ?"
    mv <- lookupEnv "PAGER"
    case mv of
      Nothing -> i N ""
      Just v  -> i Y v

    pdesc "pager is configured in the config file ?"
    let
      confgenargs = confLookup "general" conf
      mpagerarg = find ("--pager" `isPrefixOf`) confgenargs
    meconfpager <- case mpagerarg of
      Nothing -> i N "" >> return Nothing
      Just a  -> do
        i Y a
        let arg = reverse $ takeWhile (`notElem` ['=',' ']) $ reverse a
        return $ Just $ parseYNA arg

    pdesc "hledger will use a pager when needed ?"
    mpager <- findPager
    case mpager of
      Nothing    -> p N "no pager was found"
      Just pager ->
        case meconfpager of
          Just (Right No) -> p N ""
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


------------------------------------------------------------------------------

setupPretty version conf = do
  pgroup "pretty"

  pdesc "hledger supports pretty table borders ?"
  if not $ supportsPretty version then p N "hledger 1.2+ needed"
  else do
    p Y ""

    pdesc "--pretty is enabled in the config file ?"
    let
      confgenargs = confLookup "general" conf
      confpretty = isJust $ find ("--pretty" ==) confgenargs
    i (if confpretty then Y else N) ""

------------------------------------------------------------------------------

setupCompletions version = do
  pgroup "completions"

  pdesc "up to date bash completions available ?"
  if not $ supportsBashCompletions version then p N "hledger 1.41+ needed"
  else do
    p Y ""

    pdesc "bash shell completions are installed ?"
    p U ""

------------------------------------------------------------------------------

setupJournal :: Version -> IO (Either String Journal)
setupJournal version = do
  pgroup "journal"

  pdesc "a home directory journal file exists ?"
  mh <- getHomeSafe
  (ok,msg) <- case mh of
    Just h -> do
      let f = h </> journalDefaultFilename
      e <- doesFileExist f
      return (if e then Y else N, if e then f else "")
    Nothing -> return (N, "")
  i ok msg

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

  pdesc "a default journal file exists ?"
  jfile <- defaultJournalPath
  exists <- doesFileExist jfile
  let (ok, msg) = (if exists then Y else N, if exists then jfile else "")
  p ok msg

  when exists $ do
    
    when (os == "mingw32") $ do
      pdesc "default journal file path is safe for Windows ?"
      let
        (ok, msg) =
          -- like ensureJournalFileExists:
          if isWindowsUnsafeDotPath jfile
          then (N, "the file name ends with a dot, this is unsafe on Windows")
          else (Y, "")
      p ok msg

    pdesc "hledger can read the default journal ?"
    -- Basic readability check: ignoring config files if it's hledger >=1.40,
    -- and balance assertions if possible (can't if it's hledger <=0.23),
    -- try read the file (ie do the parseable and autobalanced checks pass).
    let
      args = concat [
        ["print"],
        ["--ignore-assertions" | supportsIgnoreAssertions version],
        ["--no-conf" | supportsConfigFiles version]
        ]
    (exit, _, err) <- readProcessWithExitCode progname args ""
    case exit of
      ExitSuccess   -> p Y ""
      ExitFailure _ -> p N ("\n"<>err)

  -- Try to read the journal ourselves for closer inspection later.
  -- There's the possibility this could read the journal differently from the hledger in PATH,
  -- if that's  a different version.
  -- Also we assume defaultJournalPath will detect the same file as the logic above.
  defaultJournalSafely


------------------------------------------------------------------------------

setupAccounts version mj = do
  pgroup "accounts"

  pdesc "hledger supports account directives ?"
  if not $ supportsAccountDirective version
  then p N "hledger 1.9+ needed"
  else do
    p Y ""

    pdesc "hledger supports all account types ?"  --  (ALERXCV)
    if not $ supportsConversionAccountType version
    then p N "hledger 1.25+ needed"
    else do
      p Y ""

      case mj of
        Nothing -> return ()
        Just j@Journal{..} -> do

          let
            accttypes = [Asset, Liability, Equity, Revenue, Expense, Cash, Conversion]
            typesdeclaredorinferred = nub $ M.elems jaccounttypes
            acctswithdeclaredorinferredtype = nub (M.keys jaccounttypes)
            untypedaccts = journalAccountNames j \\ acctswithdeclaredorinferredtype
            undeclaredaccts = journalAccountNamesUsed j \\ journalAccountNamesDeclared j
            hasdeclaredaccts t = case M.lookup t jdeclaredaccounttypes of
              Just (_ : _) -> True
              _ -> False

          pdesc "Asset accounts declared ?"    
          if hasdeclaredaccts Asset then i Y "" else i N ""

          pdesc "Liability accounts declared ?"
          if hasdeclaredaccts Liability then i Y "" else i N ""

          pdesc "Equity accounts declared ?"
          if hasdeclaredaccts Equity then i Y "" else i N ""

          pdesc "Revenue accounts declared ?"
          if hasdeclaredaccts Revenue then i Y "" else i N ""

          pdesc "Expense accounts declared ?"
          if hasdeclaredaccts Expense then i Y "" else i N ""

          pdesc "Cash accounts declared ?"
          if hasdeclaredaccts Cash then i Y "" else i N ""

          pdesc "Conversion accounts declared ?"
          if hasdeclaredaccts Conversion then i Y "" else i N ""  -- ("--infer-equity will use a default conversion account name")

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

          pdesc "an account of each type was detected ?"
          if all (`elem` typesdeclaredorinferred) accttypes
          then p Y ""
          else p N "reports like bs, cf, is may be empty"

          pdesc "all accounts have types ?"
          if null untypedaccts then i Y "" else i N ""

          pdesc "all accounts are declared ?"
          if null undeclaredaccts then i Y "" else i N ""

------------------------------------------------------------------------------

setupCommodities version mj = do
  pgroup "commodities"

  pdesc "hledger supports commodity directives ?"
  if not $ supportsCommodityDirective version
  then p N "hledger 1.0+ needed"
  else do
    p Y ""

    case mj of
      Nothing -> return ()
      Just j -> do

        pdesc "all commodities are declared ?"
        let undeclaredcommodities = journalCommoditiesUsed j \\ journalCommoditiesDeclared j
        if null undeclaredcommodities then p Y "" else p N "declaring helps set their precision"

------------------------------------------------------------------------------

-- setupX = do
--   pgroup "x"
--   pdesc "x ?"

------------------------------------------------------------------------------

-- yes, no, unknown
data YNU = Y | N | U deriving (Eq)

-- ANSI styles
good    = bold' . brightGreen'
neutral = bold' . brightBlue'
warning = bold' . brightYellow'
bad     = bold' . brightRed'

-- Show status, in red/green/yellow if supported.
instance Show YNU where
  show Y = good    "yes"    -- ✅ apple emojis - won't work everywhere
  show N = bad     " no"    -- ❌
  show U = warning " ? "

-- Show status, in blue/yellow if supported.
showInfo Y = neutral "yes"  -- ℹ️
showInfo N = neutral " no"  -- ℹ️
showInfo U = warning " ? "

-- | Print a test's pass or fail status, as "yes" or "no" or "",
-- in green/red if supported, and the (possibly empty) provided message.
p :: YNU -> String -> IO ()
p ok msg = putStrLn $ unwords ["", show ok, "", msg]

-- | Like p, but display the status as info, in neutral blue.
i :: YNU -> String -> IO ()
i ok msg = putStrLn $ unwords ["", showInfo ok, "", msg]

-- | Print a setup test groups heading.
pgroup :: String -> IO ()
pgroup s = putStrLn $ "\n" <> bold' s

-- | Print a setup test's description, formatting and padding it to a fixed width.
pdesc :: String -> IO ()
pdesc s = printf "* %-40s" s

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
