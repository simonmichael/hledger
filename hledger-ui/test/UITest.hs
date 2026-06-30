{-# LANGUAGE OverloadedStrings #-}

-- | Proof-of-concept automated tests for hledger-ui, driving the real brick/vty
-- event loop headlessly. See "UITestUtils" for how the harness works and its limits.
-- These exercise the behaviours we most want to protect during refactoring:
-- drill-down, backtracking that preserves each screen's selection, and reload (#1825).

module Main (main) where

import Control.Exception (finally)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Calendar (fromGregorian)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hledger (Journal, readJournalFile')
import Hledger.UI.UIOptions (UIOpts)
import UITestUtils

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "hledger-ui"
  [ testCase "drill-down builds the expected screen stack" $
      withFixture $ \uopts j -> do
        (states, _) <- driveUI uopts j [keyRight, keyRight]
        -- menu -> accounts -> register -> transaction
        screenPath (last states) @?= "MART"

  , testCase "backtracking preserves the accounts-screen selection" $
      withFixture $ \uopts j -> do
        (states, _) <- driveUI uopts j [keyDown, keyDown, keyRight, keyLeft]
        let afterDowns = states !! 2  -- accounts screen, selection moved down twice
            afterBack  = last states  -- popped back to the accounts screen
        activeScreenTag     afterDowns @?= "A"
        activeScreenTag     afterBack  @?= "A"
        selectedItemAccount afterBack  @?= selectedItemAccount afterDowns
        selectedIndex       afterBack  @?= selectedIndex       afterDowns

  , testCase "reload preserves the accounts-screen selection (#1825)" $
      withFixture $ \uopts j -> do
        --                          down  down   right(reg) g(reload) left(back)
        (states, _) <- driveUI uopts j [keyDown, keyDown, keyRight, key 'g', keyLeft]
        let beforeReload = states !! 2  -- accounts screen before drilling in
            afterReload  = last states  -- back on the accounts screen after a reload
        activeScreenTag     afterReload @?= "A"
        selectedItemAccount afterReload @?= selectedItemAccount beforeReload
        selectedIndex       afterReload @?= selectedIndex       beforeReload

  , testCase "register screen renders descriptions and amounts" $
      withFixture $ \uopts j -> do
        -- down twice selects expenses:food, then right enters its register
        (states, frames) <- driveUI uopts j [keyDown, keyDown, keyRight]
        activeScreenTag (last states) @?= "R"
        -- scope to the register frame itself (the accounts screen also shows balances)
        let reg = renderText region (last frames)
            hasText t = any (T.isInfixOf t) reg
        -- a description, rendered directly from the list ...
        assertBool "the register should show the 'grocery' transaction description"
          (hasText "grocery")
        -- ... and an amount, rendered via the amount-column width calculation
        assertBool "the register should show the $20 amount"
          (hasText "$20")
  ]

-- | Run an action with a freshly loaded fixture journal and matching startup
-- options (the all-accounts screen, with a pinned report date for reproducibility).
withFixture :: (UIOpts -> Journal -> IO a) -> IO a
withFixture act =
  withJournalFile fixture $ \path -> do
    uopts <- withDay (fromGregorian 2024 6 1) <$> uiOptsForArgs ["-f", path, "--all"]
    j <- readJournalFile' path
    act uopts j

-- A small date-stable journal: absolute dates, no forecasting.
fixture :: Text
fixture = T.unlines
  [ "2024-01-05 opening"
  , "    assets:checking      $1000"
  , "    equity:opening"
  , ""
  , "2024-02-10 grocery"
  , "    expenses:food          $20"
  , "    assets:checking"
  , ""
  , "2024-03-15 salary"
  , "    assets:checking       $500"
  , "    income:salary"
  ]

withJournalFile :: Text -> (FilePath -> IO a) -> IO a
withJournalFile content act = do
  tmp <- getTemporaryDirectory
  (path, h) <- openTempFile tmp "hledger-ui-test.journal"
  T.hPutStr h content >> hClose h
  act path `finally` removeFile path
