-- | /journal handlers.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Handler.JournalR where

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Web.Import
import Hledger.Web.WebOptions
import Hledger.Web.Widget.AddForm (addModal)
import Hledger.Web.Widget.Common
            (accountQuery, mixedAmountAsHtml,
             transactionFragment, replaceInacct)

-- | The formatted journal view, with sidebar.
getJournalR :: Handler Html
getJournalR = do
  checkServerSideUiEnabled
  VD{caps, j, m, opts, q, qopts, today} <- getViewData
  when (CapView `notElem` caps) (permissionDenied "Missing the 'view' capability")
  let title = case inAccount qopts of
        Nothing -> "General Journal"
        Just (a, inclsubs) -> "Transactions in " <> a <> if inclsubs then "" else " (excluding subaccounts)"
      title' = title <> if m /= Any then ", filtered" else ""
      acctlink a = (RegisterR, [("q", replaceInacct q $ accountQuery a)])
      rspec = (reportspec_ $ cliopts_ opts){_rsQuery = filterQuery (not . queryIsDepth) m}
      items = reverse $ entriesReport rspec j
      transactionFrag = transactionFragment j

  defaultLayout $ do
    setTitle "journal - hledger-web"
    $(widgetFile "journal")
