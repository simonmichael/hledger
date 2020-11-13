{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

-- cabal missing-home-modules workaround from hledger-lib needed here ?
-- {-# LANGUAGE PackageImports #-}

import Test.Hspec (hspec)
import Yesod.Default.Config
import Yesod.Test

import Hledger.Web.Application (makeFoundation)
import Hledger.Web.Foundation
import Hledger.Web.Settings (parseExtra)
import Hledger.Web.WebOptions -- (defwebopts, cliopts_)
-- import Hledger.Cli.CliOptions -- (defcliopts, reportspec_)
-- import Hledger  -- .Reports.ReportOptions (defreportopts, forecast_)

main :: IO ()
main = do
  conf <- Yesod.Default.Config.loadConfig $ 
            (configSettings Testing){ csParseExtra = parseExtra }

  foundation <- makeFoundation conf defwebopts
  hspec $ yesodSpec foundation specs

  -- run hledger-web with some forecasted transactions
  -- XXX problem: these tests use makeFoundation, bypassing the journal setup in Hledger.Web.Main
  -- d <- getCurrentDay
  -- let
  --   ropts = defreportopts{forecast_=Just nulldatespan}
  --   rspec = case reportOptsToSpec d ropts of
  --             Left e   -> error $ "failed to set up report options for tests, shouldn't happen: " ++ show e
  --             Right rs -> rs
  -- foundationForecast <- makeFoundation conf 
  --   defwebopts{cliopts_=defcliopts{file_=["hledger-web/tests/forecast.j"], reportspec_=rspec}}
  -- hspec $ yesodSpec foundationForecast specsForecast

-- https://hackage.haskell.org/package/yesod-test/docs/Yesod-Test.html

specs :: YesodSpec App
specs = do
  ydescribe "hledger-web basic functionality" $ do

    yit "serves a reasonable-looking journal page" $ do
      get JournalR
      statusIs 200
      bodyContains "Add a transaction"

    yit "serves a reasonable-looking register page" $ do
      get RegisterR
      statusIs 200
      bodyContains "accounts"

-- specsForecast :: YesodSpec App
-- specsForecast = do
--   ydescribe "hledger-web --forecast" $ do
--     yit "serves a reasonable-looking journal page" $ do
--       get JournalR
--       statusIs 200
--       bodyContains "Add a transaction"




-- post "/" $ do
--   addNonce
--   fileByLabel "Choose a file" "tests/main.hs" "text/plain" -- talk about self-reference
--   byLabel "What's on the file?" "Some Content"
-- statusIs 200
-- htmlCount ".message" 1
-- htmlAllContain ".message" "Some Content"
-- htmlAllContain ".message" "text/plain"
