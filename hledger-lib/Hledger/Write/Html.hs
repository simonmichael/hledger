{- |
HTML writing helpers.
This module would ideally hide the details of which HTML library is used, but it doesn't yet.

Currently hledger-web uses blaze-html, but hledger CLI reports use lucid.
lucid has a more usable API than blaze-html (https://chrisdone.com/posts/lucid).
lucid2's is even better.
Unfortunately lucid* can not render multi-line or indented text.
We want this so that humans can read and troubleshoot our HTML output.
So a transition to blaze-html may be coming.

-}

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Write.Html (
  L.toHtml,
  Html,
  formatRow,
  htmlAsText,
  htmlAsLazyText,
  styledTableHtml,
  tests_Hledger_Write_Html
  ) where

import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as TL (Text, toStrict)
import qualified Lucid as L (renderText, toHtml)
import Test.Tasty (testGroup)

import Hledger.Write.Html.Lucid (Html, formatRow, styledTableHtml)


htmlAsText :: Html -> T.Text
htmlAsText = TL.toStrict . L.renderText

htmlAsLazyText :: Html -> TL.Text
htmlAsLazyText = L.renderText

tests_Hledger_Write_Html = testGroup "Write.Html" [
  ]
