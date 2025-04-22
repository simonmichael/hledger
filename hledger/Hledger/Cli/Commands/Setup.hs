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
-- {-# OPTIONS_GHC -Wno-unused-matches #-}

module Hledger.Cli.Commands.Setup (
  setupmode
 ,setup
)
where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
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

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Conf
import Hledger.Cli.Version (Version, toVersion)


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
  mversion <- setupHledger
  case mversion of
    Nothing -> return ()
    Just (_, version) -> do
      setupConfig version
      setupFile version
      -- setupAccounts version
      -- setupCommodities version
      -- setupTags version
      return ()
  putStr "\n"

-- Test a hledger version for support of various features.
supportsIgnoreAssertions      = (>= 0 :| [24])  -- --ignore-assertions, 2014
supportsAccountTypes          = (>= 1 :| [13])  -- ALERX account types, type: tag, 2019
supportsCashAccountType       = (>= 1 :| [19])  -- C/Cash account type, 2020
supportsConversionAccountType = (>= 1 :| [25])  -- V/Conversion account type, accounts --types, 2022
supportsConfigFiles           = (>= 1 :| [40])  -- config files, 2024

------------------------------------------------------------------------------

-- | This first test group looks for a "hledger" executable in PATH;
-- if found, tests it in various ways;
-- and if it ran successfully, returns the full --version output
-- and the numeric Version parsed from that.
setupHledger :: IO (Maybe (String, Version))
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

      -- If hledger was found in PATH, do more checks

      pdesc "runs ?"
      eerrout <- tryHledgerArgs [["--version", "--no-conf"], ["--version"]]
      case eerrout of
        Left  err -> p N ("'" <> progname <> " --version' failed: \n" <> err) >> return Nothing
        Right out -> do
          p Y ""

          -- If it runs, do more checks
          let
            versionoutput = rstrip out
            versionwords = words versionoutput

          pdesc "is a native binary ?"
          let
            sysarch = os' <> "-" <> arch
              where
                os' -- keep synced: Version.hs
                  | os == "darwin" = "mac"
                  | os == "mingw32" = "windows"
                  | otherwise = os
          case drop 2 versionwords of
            exearch:_ -> if exearch == sysarch
              then p Y versionoutput
              else p N $ "installed binary is for " <> exearch <> ", system is " <> sysarch
            _ -> p U $ "couldn't detect arch in --version output"

          pdesc "is up to date ?"
          case drop 1 versionwords of
            [] -> p U "couldn't parse --version output" >> return Nothing
            detailedversionstr:_ -> do
              let
                versionnumstr = takeWhile (`elem` ("0123456789." :: String)) detailedversionstr
                mversion      = toVersion versionnumstr
              case mversion of
                Nothing -> p U "couldn't parse --version's version number" >> return Nothing
                Just version -> do
                  elatestversionnumstr <- getLatestHledgerVersion
                  case elatestversionnumstr of
                    Left e -> p U ("couldn't read " <> latestHledgerVersionUrlStr <> " , " <> e)
                    Right latestversionnumstr ->
                      case toVersion latestversionnumstr of
                        Nothing -> p U "couldn't parse latest version number"
                        Just latest -> p (if version >= latest then Y else N) (versionnumstr <> " installed, latest is " <> latestversionnumstr)
                  return $ Just (versionoutput, version) 


------------------------------------------------------------------------------

setupConfig version = do
  pgroup "config"

  pdesc "this hledger supports config files ?"
  if (not $ supportsConfigFiles version)
  then p N "hledger 1.40+ needed"
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
      Nothing -> return ()
      Just _ -> do
        pdesc "this hledger can read the config file ?"
        -- Test config file readability, without requiring journal file readability, forward compatibly.
        (exit, _, err) <- readProcessWithExitCode progname ["print", "-f-"] ""
        case exit of
          ExitSuccess   -> p Y ""
          ExitFailure _ -> p N ("\n"<>err)

    -- pdesc "common general options configured ?"
    -- --pretty --ignore-assertions --infer-costs"
    -- print --explicit --show-costs"

------------------------------------------------------------------------------

setupFile version = do
  pgroup "file"

  pdesc "a home directory journal file exists ?"
  mh <- getHomeSafe
  (ok,msg) <- case mh of
    Just h -> do
      let f = h </> journalDefaultFilename
      e <- doesFileExist f
      return (if e then Y else N, if e then f else "")
    Nothing -> return (N, "")
  i ok msg

  pdesc "LEDGER_FILE variable is defined ?"
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

  pdesc "default journal file exists ?"
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

    pdesc "this hledger can read default journal ?"
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


------------------------------------------------------------------------------

setupAccounts = do
  pgroup "accounts"

  pdesc "all account types declared or detected ?"

  -- pdesc "  asset, liability, equity, revenue, expense, cash, conversion"

  -- pdesc "untyped accounts ?"

  -- pdesc "all used accounts declared ?"

------------------------------------------------------------------------------

setupCommodities = do
  pgroup "commodities"
  -- pdesc "all used commodities declared ?"
  -- pdesc "\n"
  -- pdesc "\n"

------------------------------------------------------------------------------

setupTags = do
  pgroup "tags"
  -- pdesc "all used tags declared ?"
  -- pdesc "\n"
  -- pdesc "\n"

------------------------------------------------------------------------------

-- yes, no, unknown
data YNU = Y | N | U deriving (Eq)

-- Show status, in red/green/yellow if supported.
instance Show YNU where
  show Y = bold' (brightGreen'  "yes")    -- ✅ apple emojis - won't work everywhere
  show N = bold' (brightRed'    " no")    -- ❌
  show U = bold' (brightYellow' " ? ")

-- Show status, in blue/yellow if supported.
showInfo Y = bold' (brightBlue'   "yes")  -- ℹ️
showInfo N = bold' (brightBlue'   " no")  -- ℹ️
showInfo U = bold' (brightYellow' " ? ")

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
