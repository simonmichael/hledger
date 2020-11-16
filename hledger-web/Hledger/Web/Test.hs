module Hledger.Web.Test (
  hledgerWebTest
) where

import qualified Data.Text as T
import Test.Hspec (hspec)
import Yesod.Default.Config
import Yesod.Test

import Hledger.Web.Application ( makeFoundationWith )
import Hledger.Web.WebOptions ( WebOpts(cliopts_), defwebopts )
import Hledger.Web.Import hiding (get, j)
import Hledger.Cli hiding (tests)


runHspecTestsWith :: AppConfig DefaultEnv Extra -> WebOpts -> Journal -> YesodSpec App -> IO ()
runHspecTestsWith yesodconf hledgerwebopts j specs = do
  app <- makeFoundationWith j yesodconf hledgerwebopts
  hspec $ yesodSpec app specs

-- Run hledger-web's built-in tests using the hspec test runner.
hledgerWebTest :: IO ()
hledgerWebTest = do
  putStrLn $ "Running tests for " ++ prognameandversion -- ++ " (--test --help for options)"

  conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing){ csParseExtra = parseExtra }

  -- https://hackage.haskell.org/package/yesod-test-1.6.10/docs/Yesod-Test.html
  -- http://hspec.github.io/writing-specs.html
  --
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

