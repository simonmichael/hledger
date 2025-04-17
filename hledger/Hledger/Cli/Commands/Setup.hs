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

import Control.Exception
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Network.HTTP.Client
import Network.HTTP.Types (statusCode, hLocation)
import Network.HTTP.Req
import Safe
import System.Directory
import System.Exit
import System.FilePath
import System.Info
import System.IO
import System.Process

import Hledger
import Hledger.Cli.CliOptions

setupmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Setup.txt")
  []
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | Check and show the status of various aspects of the hledger installation.
-- Also show extra info and hints on how to fix problems.
setup :: CliOpts -> Journal -> IO ()
setup _opts@CliOpts{rawopts_=_rawopts, reportspec_=_rspec} _ignoredj = do
  -- This command is not given a journal and should not use _ignoredj;
  -- instead detect it ourselves when we are ready.
  putStrLn "checking setup..."
  setupHledger
  -- setupConfig
  -- setupFiles
  -- setupAccounts
  -- setupCommodities
  -- setupTags

-- | Print a check's status, and optional messages if it's ok or not ok.
p :: Bool -> String -> String -> IO ()
p ok ymsg nmsg =
  putStrLn $ unwords $ if ok then ["", y, "", ymsg] else ["", n, "", nmsg]
  where
    y = "yes ✅"
    n = "no ❌"

setupHledger :: IO ()
setupHledger = do
  putStrLn "hledger:"

  putStr "- is in PATH ?"
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
    ok = not $ null pathexes
    pathexe  = quoteIfNeeded $ headDef (error' "showing nonexistent executable") pathexes
    otherexe = quoteIfNeeded $ headDef (error' "showing nonexistent executable") otherexes
    otherdir = takeDirectory otherexe
    hint = if null otherexes
      then ("Add " <> progname <> "'s directory to your shell's PATH.")
      else unlines
        ["Add " <> otherdir <> " to PATH in your shell config."
        ,"  Eg on unix: echo 'export PATH=" <> otherdir <> ":$PATH' >> ~/.profile"
        ,"  and start a new shell session."
        ]
  p ok pathexe hint

  -- If hledger was found in PATH, run more checks
  when ok $ do

    putStr "- runs ?"
    let arg = "--version"
    (exit,out,err) <- readProcessWithExitCode progname [arg] ""
    let
      ok = exit == ExitSuccess
      hint = "'" <> progname <> " " <> arg <> "' failed: \n" <> err
    p ok "" hint
    let verparts = words out  -- use below

    -- If hledger runs, run more checks
    when ok $ do
      putStr "- is a native binary ?"
      let
        exearch = case drop 2 verparts of
          w:_ -> w
          _   -> error' "couldn't parse arch from --version output"
        os'  -- keep synced: Version.hs
          | os == "darwin"  = "mac"
          | os == "mingw32" = "windows"
          | otherwise       = os
        sysarch = os' <> "-" <> arch
        ok = exearch == sysarch
        hint = "installed binary is for " <> exearch <> ", system is " <> sysarch
      p ok "" hint

      putStr "- is up to date ?"
      elatestver <- getLatestHledgerVersion
      let
        latestver = case elatestver of
          Left e -> error' $ "failed to detect latest hledger version: " <> e
          Right v -> v
        exedetailedver = case drop 1 verparts of
          w:_ -> w
          _   -> error' "couldn't parse detailed version from --version output"
        exever = takeWhile (`elem` ("0123456789."::String)) exedetailedver
        ok = splitAtElement '.' exever >= splitAtElement '.' latestver
        msg
          | exever == latestver = exever
          | otherwise = exever <> " installed, latest release is " <> latestver
      p ok msg msg

  -- putStr "- eget installed ?"

setupConfig = do
  putStrLn "config:"
  -- putStr "- user config file exists ?"
  -- putStr "\n"
  -- putStr "- local config masking user config ?"
  -- putStr "\n"
  -- putStr "- config file readable ?"
  -- putStr "\n"
  -- putStr "- common general options configured ?"
  -- putStr "\n"
  -- putStr "  --pretty --ignore-assertions --infer-costs"
  -- putStr "\n"
  -- putStr "  print --explicit --show-costs"
  -- putStr "\n"
  -- putStr "\n"

setupFiles = do
  putStrLn "files:"
  -- putStr "- default journal file exists ?"
  -- putStr "\n"
  -- putStr "- default journal file readable ?"
  -- putStr "\n"
  -- putStr "\n"

setupAccounts = do
  putStrLn "accounts:"
  -- putStr "- all account types declared or detected ?"
  -- putStr "\n"
  -- putStr "  asset, liability, equity, revenue, expense, cash, conversion"
  -- putStr "\n"
  -- putStr "- untyped accounts ?"
  -- putStr "\n"
  -- putStr "- all used accounts declared ?"
  -- putStr "\n"
  -- putStr "\n"

setupCommodities = do
  putStrLn "commodities:"
  -- putStr "- all used commodities declared ?"
  -- putStr "\n"
  -- putStr "\n"

setupTags = do
  putStrLn "tags:"
  -- putStr "- all used tags declared ?"
  -- putStr "\n"
  -- putStr "\n"

getLatestHledgerVersion :: IO (Either String String)
getLatestHledgerVersion = do
  result <- try $ runReq defaultHttpConfig{httpConfigRedirectCount=0} $
    req GET (https "hackage.haskell.org" /: "package" /: "hledger" /: "docs" /: "") NoReqBody bsResponse mempty
  case result of
    Right _ -> return $ Left "no redirect"
    Left (VanillaHttpException (HttpExceptionRequest _ (StatusCodeException rsp _))) -> do
      let status = statusCode $ responseStatus rsp
      if status >= 300 && status < 400
        then do
          let locationHeader = lookup hLocation (responseHeaders rsp)
          case fmap T.decodeUtf8 locationHeader of
            Nothing       -> return $ Left "redirect response with no Location header"
            Just location -> do
              let packagename = take 1 $ drop 1 $ reverse $ T.splitOn "/" location
              case packagename of
                [n] -> return $ Right $ dropWhile (`notElem` ['0'..'9']) $ T.unpack n
                _   -> return $ Left "failed to parse version from Location header"
        else return $ Left $ "non-redirect status code: " ++ show status
    Left err -> return $ Left $ "other exception: " ++ show err

{- | Ensure there is a journal file at the given path, creating an empty one if needed.
On Windows, also ensure that the path contains no trailing dots
which could cause data loss (see 'isWindowsUnsafeDotPath').
-}
_ensureJournalFileExists :: FilePath -> IO ()
_ensureJournalFileExists f = do
  when (os == "mingw32" && isWindowsUnsafeDotPath f) $
    error' $
      "Part of file path \"" <> show f <> "\"\n ends with a dot, which is unsafe on Windows; please use a different path.\n"
  exists <- doesFileExist f
  unless exists $ do
    hPutStrLn stderr $ "Creating hledger journal file " <> show f
    -- note Hledger.Utils.UTF8.* do no line ending conversion on windows,
    -- we currently require unix line endings on all platforms.
    newJournalContent >>= T.writeFile f

{- | Does any part of this path contain non-. characters and end with a . ?
Such paths are not safe to use on Windows (cf #1056).
-}
isWindowsUnsafeDotPath :: FilePath -> Bool
isWindowsUnsafeDotPath = any (\x -> last x == '.' && any (/= '.') x) . splitDirectories

-- | Give the content for a new auto-created journal file.
newJournalContent :: IO Text
newJournalContent = do
  d <- getCurrentDay
  return $ "; journal created " <> T.pack (show d) <> " by hledger\n"
