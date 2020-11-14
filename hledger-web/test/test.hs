{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- cabal missing-home-modules workaround from hledger-lib needed here ?
-- {-# LANGUAGE PackageImports #-}

module Main where

import qualified Data.Text as T
import Test.Hspec (hspec)
import Yesod.Default.Config
import Yesod.Test

import Hledger.Web
import Hledger.Web.Application
-- import Hledger.Web.Foundation
import Hledger.Web.Import hiding (get, j)
import Hledger.Cli hiding (tests)


runTestsWith :: AppConfig DefaultEnv Extra -> WebOpts -> Journal -> YesodSpec App -> IO ()
runTestsWith yesodconf hledgerwebopts j specs = do
  app <- makeFoundationWith j yesodconf hledgerwebopts
  hspec $ yesodSpec app specs

main :: IO ()
main = do

  -- https://hackage.haskell.org/package/yesod-test-1.6.10/docs/Yesod-Test.html
  -- http://hspec.github.io/writing-specs.html

  -- XXX these tests use makeFoundation, bypassing the startup code in Hledger.Web.Main
  
  -- Be careful about the opts/files provided here, unusual combinations might cause problems.
  -- Eg journalReload can reload the user's default journal if cliopts{file_} is left empty.

  conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing){ csParseExtra = parseExtra }

  -- basic tests
  runTestsWith conf defwebopts nulljournal $ do
    ydescribe "hledger-web" $ do

      yit "serves a reasonable-looking journal page" $ do
        get JournalR
        statusIs 200
        bodyContains "Add a transaction"

      yit "serves a reasonable-looking register page" $ do
        get RegisterR
        statusIs 200
        bodyContains "accounts"

  -- test with forecasted transactions
  d <- getCurrentDay
  let
    ropts = defreportopts{forecast_=Just nulldatespan}
    rspec = case reportOptsToSpec d ropts of
              Left e   -> error $ "failed to set up report options for tests, shouldn't happen: " ++ show e
              Right rs -> rs
    copts = defcliopts{reportspec_=rspec, file_=[""]}  -- non-empty, see file_ note above
    wopts = defwebopts{cliopts_=copts}
  j <- fmap (journalTransform copts) $ readJournal' (T.unlines  -- PARTIAL: readJournal' should not fail
    ["~ monthly"
    ,"    assets    10"
    ,"    income"
    ])
  runTestsWith conf wopts j $ do
    ydescribe "hledger-web --forecast" $ do

      yit "serves a journal page showing forecasted transactions" $ do
        get JournalR
        statusIs 200
        bodyContains "id=\"transaction-0-1\""  -- 0 indicates a fileless (forecasted) txn
        bodyContains "id=\"transaction-0-2\""  -- etc.

