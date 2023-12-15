{-|
Test suite for hledger-web.

Dev notes:

http://hspec.github.io/writing-specs.html

https://hackage.haskell.org/package/yesod-test-1.6.10/docs/Yesod-Test.html

"The best way to see an example project using yesod-test is to create a scaffolded Yesod project:
stack new projectname yesodweb/sqlite
(See https://github.com/commercialhaskell/stack-templates/wiki#yesod for the full list of Yesod templates)"


These tests don't exactly match the production code path, eg these bits are missing:

  withJournalDo copts (web wopts)  -- extra withJournalDo logic (journalTransform..)
  ...
  -- query logic, more options logic
  let depthlessinitialq = filterQuery (not . queryIsDepth) . _rsQuery . reportspec_ $ cliopts_ wopts
      j' = filterJournalTransactions depthlessinitialq j
      h = host_ wopts
      p = port_ wopts
      u = base_url_ wopts
      staticRoot = T.pack <$> file_url_ wopts
      appconfig = AppConfig{appEnv = Development
                           ,appHost = fromString h
                           ,appPort = p
                           ,appRoot = T.pack u
                           ,appExtra = Extra "" Nothing staticRoot
                           }

The production code path, when called in this test context, which I guess is using
yesod's dev mode, needs to read ./config/settings.yml and fails without it (loadConfig).

-}

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Web.Test (
  hledgerWebTest
) where

import Data.String (fromString)
import Data.Function ((&))
import qualified Data.Text as T
import Test.Hspec (hspec)
import Yesod.Default.Config
import Yesod.Test

import Hledger.Web.Application ( makeFoundationWith )
import Hledger.Web.WebOptions  -- ( WebOpts(..), defwebopts, prognameandversion )
import Hledger.Web.Import hiding (get, j)
import Hledger.Cli hiding (prognameandversion)


-- | Given a tests description, zero or more raw option name/value pairs,
-- a journal and some hspec tests, parse the options and configure the
-- web app more or less as we normally would (see details above), then run the tests.
--
-- Raw option names are like the long flag without the --, eg "file" or "base-url".
--
-- The journal and raw options should correspond enough to not cause problems.
-- Be cautious - without a [("file", "somepath")], perhaps journalReload could load
-- the user's default journal.
--
runTests :: String -> [(String,String)] -> Journal -> YesodSpec App -> IO ()
runTests testsdesc rawopts j tests = do
  wopts <- rawOptsToWebOpts $ mkRawOpts rawopts
  let yconf = AppConfig{  -- :: AppConfig DefaultEnv Extra
          appEnv = Testing
        -- https://hackage.haskell.org/package/conduit-extra/docs/Data-Conduit-Network.html#t:HostPreference
        -- ,appHost = "*4"  -- "any IPv4 or IPv6 hostname, IPv4 preferred"
        -- ,appPort = 3000  -- force a port for tests ?
        -- Test with the host and port from opts. XXX more fragile, can clash with a running instance ?
        ,appHost = host_ wopts & fromString
        ,appPort = port_ wopts
        ,appRoot = base_url_ wopts & T.pack  -- XXX not sure this or extraStaticRoot get used
        ,appExtra = Extra
                    { extraCopyright  = ""
                    , extraAnalytics  = Nothing
                    , extraStaticRoot = T.pack <$> file_url_ wopts
                    }
        }
  app <- makeFoundationWith j yconf wopts
  hspec $ yesodSpec app $ ydescribe testsdesc tests    -- https://hackage.haskell.org/package/yesod-test/docs/Yesod-Test.html

-- | Run hledger-web's built-in tests using the hspec test runner.
hledgerWebTest :: IO ()
hledgerWebTest = do
  putStrLn $ "Running tests for " ++ prognameandversion -- ++ " (--test --help for options)"
  let d = fromGregorian 2000 1 1

  runTests "hledger-web" [] nulljournal $ do

    yit "serves a reasonable-looking journal page" $ do
      get JournalR
      statusIs 200
      bodyContains "Add a transaction"

    yit "serves a reasonable-looking register page" $ do
      get RegisterR
      statusIs 200
      bodyContains "accounts"

    yit "hyperlinks use a base url made from the default host and port" $ do
      get JournalR
      statusIs 200
      let defaultbaseurl = defbaseurl defhost defport
      bodyContains ("href=\"" ++ defaultbaseurl)
      bodyContains ("src=\"" ++ defaultbaseurl)

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

  let
    rawopts = [("forecast","")]
    iopts = rawOptsToInputOpts d $ mkRawOpts rawopts
    f = "fake"  -- need a non-null filename so forecast transactions get index 0
  pj <- readJournal' (T.pack $ unlines  -- PARTIAL: readJournal' should not fail
    ["~ monthly"
    ,"    assets    10"
    ,"    income"
    ])
  j <- fmap (either error id) . runExceptT $ journalFinalise iopts f "" pj  -- PARTIAL: journalFinalise should not fail
  runTests "hledger-web with --forecast" rawopts j $ do

    yit "shows forecasted transactions" $ do
      get JournalR
      statusIs 200
      bodyContains "id=\"transaction-2-1\""
      bodyContains "id=\"transaction-2-2\""

  runTests "hledger-web with --base-url"
    [("base-url","https://base")] nulljournal $ do

    yit "hyperlinks respect --base-url" $ do
      get JournalR
      statusIs 200
      bodyContains "href=\"https://base"
      bodyContains "src=\"https://base"

  -- #2139
  -- runTests "hledger-web with --base-url, --file-url"
  --   [("base-url","https://base"), ("file-url","https://files")] nulljournal $ do

  --   yit "static file hyperlinks respect --file-url, others respect --base-url" $ do
  --     get JournalR
  --     statusIs 200
  --     bodyContains "href=\"https://base"
  --     bodyContains "src=\"https://files"

