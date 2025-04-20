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
-- {-# OPTIONS_GHC -Wno-unused-matches #-}

module Hledger.Cli.Commands.Setup (
  setupmode
 ,setup
)
where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad
import Data.Functor ((<&>))
import Data.Maybe
-- import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import qualified Data.Text.IO as T
import Network.HTTP.Client
import Network.HTTP.Types (statusCode, hLocation)
import Network.HTTP.Req as R
import Safe
import System.Directory
import System.Exit
import System.FilePath
import System.Info
-- import System.IO
import System.Process
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Printf (printf)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Conf
import System.Environment (lookupEnv)
import Data.Char
import Data.List

setupmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Setup.txt")
  []
  [generalflagsgroup3]
  []
  ([], Nothing)

-- | Test and print the status of various aspects of the hledger installation.
-- Also show extra info and hints on how to fix problems.
setup :: CliOpts -> Journal -> IO ()
setup _opts@CliOpts{rawopts_=_rawopts, reportspec_=_rspec} _ignoredj = do
  -- This command is not given a journal and should not use _ignoredj;
  -- instead detect it ourselves when we are ready.
  putStrLn "checking..."
  setupHledger
  setupConfig
  setupFiles
  -- setupAccounts
  -- setupCommodities
  -- setupTags
  putStr "\n"

-- | Print a setup test groups heading.
pgroup :: String -> IO ()
pgroup s = putStr $ bold' $ "\n" <> s <> ":\n"

-- | Print a setup test's description, formatting and padding it to a fixed width.
pdesc :: String -> IO ()
pdesc s = printf "- %-38s" s

-- yes, no, unknown
data YNU = Y | N | U deriving (Eq)

-- Show status, in red/green/yellow if supported.
instance Show YNU where
  show Y = bold' (brightGreen'  "yes")    -- ✅ apple emojis - won't work everywhere
  show N = bold' (brightRed'    "no ")    -- ❌
  show U = bold' (brightYellow' " ? ")

-- Show status, in blue/yellow if supported.
showInfo Y = bold' (brightBlue'   "yes")  -- ℹ️
showInfo N = bold' (brightBlue'   "no ")  -- ℹ️
showInfo U = bold' (brightYellow' " ? ")

-- | Print a test's pass or fail status, as "yes" or "no" or "",
-- in green/red if supported, and the (possibly empty) provided message.
p :: YNU -> String -> IO ()
p ok msg = putStrLn $ unwords ["", show ok, "", msg]

-- | Like p, but display the status as info, in neutral blue.
i :: YNU -> String -> IO ()
i ok msg = putStrLn $ unwords ["", showInfo ok, "", msg]


setupHledger :: IO ()
setupHledger = do
  pgroup "hledger"

  pdesc "is in PATH ?"
  pathexes <- findExecutables progname
  home <- getHomeDirectory
  appdata <- getXdgDirectory XdgData ""
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
  let
    ok = if null pathexes then N else Y
    pathexe  = quoteIfNeeded $ headDef (error' "showing nonexistent executable") pathexes
    otherexe = quoteIfNeeded $ headDef (error' "showing nonexistent executable") otherexes
    otherdir = takeDirectory otherexe
    msg
      | ok == Y = pathexe
      | null otherexes = "Add " <> progname <> "'s directory to your shell's PATH."
      | otherwise = unlines
          ["Add " <> otherdir <> " to PATH in your shell config."
          ,"  Eg on unix: echo 'export PATH=" <> otherdir <> ":$PATH' >> ~/.profile"
          ,"  and start a new shell session."
          ]
  p ok msg

  -- If hledger was found in PATH, run more checks
  when (ok==Y) $ do

    pdesc "runs ?"
    let arg = "--version"
    (exit,out,err) <- readProcessWithExitCode progname [arg] ""
    let
      ok = if exit == ExitSuccess then Y else N
      msg = if ok==Y then "" else "'" <> progname <> " " <> arg <> "' failed: \n" <> err
    p ok msg
    let verparts = words out  -- use below

    -- If hledger runs, run more checks
    when (ok==Y) $ do
      pdesc "is a native binary ?"
      let
        exearch = case drop 2 verparts of
          w:_ -> w
          _   -> error' "couldn't parse arch from --version output"
        os'  -- keep synced: Version.hs
          | os == "darwin"  = "mac"
          | os == "mingw32" = "windows"
          | otherwise       = os
        sysarch = os' <> "-" <> arch
        (ok, msg)
          | exearch == sysarch = (Y, "")
          | otherwise = (N, "installed binary is for " <> exearch <> ", system is " <> sysarch)
      p ok msg

      pdesc "is up to date ?"
      elatestver <- getLatestHledgerVersion
      let
        (ok, msg) = case elatestver of
          Left e -> (U, "could not read " <> latestHledgerVersionUrlStr <> " : " <> e)
          Right latestver ->
            case drop 1 verparts of
              []  -> (U, "could not parse --version output")
              w:_ -> (ok, msg)
                where
                  exever = takeWhile (`elem` ("0123456789."::String)) w
                  ok = if splitAtElement '.' exever >= splitAtElement '.' latestver then Y else N
                  msg =
                    if exever == latestver 
                    then exever 
                    else exever <> " installed, latest release is " <> latestver
      p ok msg

  -- pdesc "eget installed ?"

setupConfig = do
  pgroup "config"

  pdesc "user has a config file ?"
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
    Just f -> do
      pdesc "config file is readable ?"
      ecs <- readFile f <&> parseConf f . T.pack
      case ecs of
        Right _ -> p Y ""
        Left e  -> p N (errorBundlePretty e)

  -- pdesc "common general options configured ?"
  -- --pretty --ignore-assertions --infer-costs"
  -- print --explicit --show-costs"

setupFiles = do
  pgroup "file"

  pdesc "a home directory journal exists ?"
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
  --   i True "" ""

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

    pdesc "default journal file is readable ?"
    ej <- runExceptT $ readJournalFile definputopts jfile  -- like defaultJournal
    case ej of
      Right _ -> p Y ""
      Left  e -> p N e

setupAccounts = do
  pgroup "accounts"
  -- pdesc "all account types declared or detected ?"
  -- pdesc "\n"
  -- pdesc "  asset, liability, equity, revenue, expense, cash, conversion"
  -- pdesc "\n"
  -- pdesc "untyped accounts ?"
  -- pdesc "\n"
  -- pdesc "all used accounts declared ?"
  -- pdesc "\n"
  -- pdesc "\n"

setupCommodities = do
  pgroup "commodities"
  -- pdesc "all used commodities declared ?"
  -- pdesc "\n"
  -- pdesc "\n"

setupTags = do
  pgroup "tags"
  -- pdesc "all used tags declared ?"
  -- pdesc "\n"
  -- pdesc "\n"

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
                _   -> return $ Left "could not parse Location"
        else return $ Left $ "HTTP status " ++ show status
    Left err -> return $ Left $ "other exception: " ++ show err

-- | Like the above, but get the version from the first number on the hledger.org Install page.
getLatestHledgerVersionFromHledgerOrg :: IO (Either String String)
getLatestHledgerVersionFromHledgerOrg = do
  let url = https "hledger.org" /: "install.html"
  result <- try $ runReq defaultHttpConfig $
    req GET url NoReqBody bsResponse (R.responseTimeout httptimeout)
  case result of
    Left (e :: R.HttpException) -> return $ Left $ show e
    Right rsp -> case T.decodeUtf8' $ R.responseBody rsp of
      Left e  -> return $ Left $ show e
      Right t -> return $
        if null version then Left "could not parse version" else Right version
        where
          -- keep synced
          versionline = take 1 $ dropWhile (not . ("current hledger release" `isInfixOf`)) $ lines $ T.unpack t
          version = takeWhile (`elem` ("0123456789."::[Char])) $ dropWhile (not . isDigit) $ headDef "" $ versionline
