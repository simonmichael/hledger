{-# LANGUAGE OverloadedStrings #-}
module HomeTest
    ( homeSpecs
    ) where

import TestImport

homeSpecs :: Specs
homeSpecs =
  ydescribe "Some hledger-web tests" $

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
