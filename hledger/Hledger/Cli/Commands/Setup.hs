{-|

Check and show the status of the hledger installation,
show extra info and hints,
and offer to fix problems where possible.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.Cli.Commands.Setup (
  setupmode
 ,setup
)
where

import System.FilePath
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger
import Hledger.Cli.CliOptions
import Control.Monad
import System.Info
import System.Directory
import System.IO
import Safe
-- import Text.Printf (printf)


setupmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Setup.txt")
  []
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | 1. Check and show the status of various aspects of the hledger installation.
-- 2. Show extra info and hints on how to fix problems.
-- 3. When possible, offer to help fix problems, interactively.
setup :: CliOpts -> Journal -> IO ()
setup _opts@CliOpts{rawopts_=_rawopts, reportspec_=_rspec} _ignoredj = do
  -- This command is not given a journal and should not use _ignoredj;
  -- instead detect it ourselves when we are ready.
  let
    p ok ymsg nmsg =
      putStrLn $ unwords $ if ok then ["", y, "", ymsg] else ["", n, "", nmsg]
      where
        y = "yes ✅"
        n = "no ❌"

  putStrLn "hledger:"

  putStr "- in PATH ?"
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

  -- putStr "- runnable ?"

  -- putStr "- up to date ?"

  -- putStr "- native binary ?"

  -- putStr "- eget installed ?"

  putStr "\n"

  -- putStrLn "config:"
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

  -- putStrLn "files:"
  -- putStr "- default journal file exists ?"
  -- putStr "\n"
  -- putStr "- default journal file readable ?"
  -- putStr "\n"
  -- putStr "\n"

  -- putStrLn "accounts:"
  -- putStr "- all account types declared or detected ?"
  -- putStr "\n"
  -- putStr "  asset, liability, equity, revenue, expense, cash, conversion"
  -- putStr "\n"
  -- putStr "- untyped accounts ?"
  -- putStr "\n"
  -- putStr "- all used accounts declared ?"
  -- putStr "\n"
  -- putStr "\n"

  -- putStrLn "commodities:"
  -- putStr "- all used commodities declared ?"
  -- putStr "\n"
  -- putStr "\n"

  -- putStrLn "tags:"
  -- putStr "- all used tags declared ?"
  -- putStr "\n"
  -- putStr "\n"

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
