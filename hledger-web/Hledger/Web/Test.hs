{-# LANGUAGE OverloadedStrings #-}

module Hledger.Web.Test (
  hledgerWebTest
) where

import qualified Data.Text as T
import Test.Hspec (hspec)
import Yesod.Default.Config
import Yesod.Test

import Hledger.Web.Application ( makeFoundationWith )
import Hledger.Web.WebOptions ( WebOpts(cliopts_), defwebopts, prognameandversion )
import Hledger.Web.Import hiding (get, j)
import Hledger.Cli hiding (prognameandversion, tests)


runHspecTestsWith :: AppConfig DefaultEnv Extra -> WebOpts -> Journal -> YesodSpec App -> IO ()
runHspecTestsWith yesodconf hledgerwebopts j specs = do
  app <- makeFoundationWith j yesodconf hledgerwebopts
  hspec $ yesodSpec app specs

-- Run hledger-web's built-in tests using the hspec test runner.
hledgerWebTest :: IO ()
hledgerWebTest = do
  putStrLn $ "Running tests for " ++ prognameandversion -- ++ " (--test --help for options)"

  -- loadConfig fails without ./config/settings.yml; use a hard-coded one
  let conf = AppConfig{
               appEnv = Testing
              ,appPort = 3000  -- will it clash with a production instance ? doesn't seem to
              ,appRoot = "http://localhost:3000"
              ,appHost = "*4"
              ,appExtra = Extra
                          { extraCopyright  = ""
                          , extraAnalytics  = Nothing
                          , extraStaticRoot = Nothing
                          }
                  }

  -- http://hspec.github.io/writing-specs.html
  -- https://hackage.haskell.org/package/yesod-test-1.6.10/docs/Yesod-Test.html
  -- "The best way to see an example project using yesod-test is to create a scaffolded Yesod project:
  -- stack new projectname yesodweb/sqlite
  -- (See https://github.com/commercialhaskell/stack-templates/wiki#yesod for the full list of Yesod templates)"

  -- Since these tests use makeFoundation, the startup code in Hledger.Web.Main is not tested. XXX
  --
  -- Be aware that unusual combinations of opts/files here could cause problems,
  -- eg if cliopts{file_} is left empty journalReload might reload the user's default journal.

  -- basic tests
  runHspecTestsWith conf defwebopts nulljournal $ do
    ydescribe "hledger-web" $ do

      yit "serves a reasonable-looking journal page" $ do
        get JournalR
        statusIs 200
        bodyContains "Add a transaction"

      yit "serves a reasonable-looking register page" $ do
        get RegisterR
        statusIs 200
        bodyContains "accounts"

      -- WIP
      -- yit "shows the add form" $ do
      --   get JournalR
      --   -- printBody
      --   -- let addbutton = "button:contains('add')"
      --   -- bodyContains addbutton
      --   -- htmlAnyContain "button:visible" "add"
      --   printMatches "div#addmodal:visible"
      --   htmlCount "div#addmodal:visible" 0

      --   -- clickOn "a#addformlink"
      --   -- printBody
      --   -- bodyContains addbutton

      -- yit "can add transactions" $ do

  -- test with forecasted transactions
  d <- getCurrentDay
  let
    ropts = defreportopts{forecast_=Just nulldatespan}
    rspec = case reportOptsToSpec d ropts of
            Left e   -> error $ "failed to set up report options for tests, shouldn't happen: " ++ show e
            Right rs -> rs
    copts = defcliopts{reportspec_=rspec, file_=[""]}  -- non-empty, see file_ note above
    wopts = defwebopts{cliopts_=copts}
  j <- fmap (journalTransform copts) $ readJournal' (T.pack $ unlines  -- PARTIAL: readJournal' should not fail
    ["~ monthly"
    ,"    assets    10"
    ,"    income"
    ])
  runHspecTestsWith conf wopts j $ do
    ydescribe "hledger-web --forecast" $ do

      yit "serves a journal page showing forecasted transactions" $ do
        get JournalR
        statusIs 200
        bodyContains "id=\"transaction-0-1\""  -- 0 indicates a fileless (forecasted) txn
        bodyContains "id=\"transaction-0-2\""  -- etc.

