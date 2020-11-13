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
import Hledger.Web.WebOptions (defwebopts)

main :: IO ()
main = do
  conf <- Yesod.Default.Config.loadConfig $ 
            (configSettings Testing){ csParseExtra = parseExtra }
  foundation <- makeFoundation conf defwebopts
  hspec $ yesodSpec foundation specs

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

      -- post "/" $ do
      --   addNonce
      --   fileByLabel "Choose a file" "tests/main.hs" "text/plain" -- talk about self-reference
      --   byLabel "What's on the file?" "Some Content"

      -- statusIs 200
      -- htmlCount ".message" 1
      -- htmlAllContain ".message" "Some Content"
      -- htmlAllContain ".message" "text/plain"

