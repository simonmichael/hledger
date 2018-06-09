-- | /journal handlers.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.JournalR where

import Import

import Handler.Common (accountQuery, mixedAmountAsHtml)

import Hledger.Cli.CliOptions
import Hledger.Data
import Hledger.Query
import Hledger.Reports
import Hledger.Utils
import Hledger.Web.WebOptions

-- | The formatted journal view, with sidebar.
getJournalR :: Handler Html
getJournalR = do
  VD{j, m, opts, qopts} <- getViewData
  -- XXX like registerReportAsHtml

  let title = case inAccount qopts of
        Nothing -> "General Journal"
        Just (a, inclsubs) -> "Transactions in " <> a <> if inclsubs then "" else " (excluding subaccounts)"
      title' = title <> if m /= Any then ", filtered" else ""
      acctlink a = (RegisterR, [("q", accountQuery a)])
      (_, items) = journalTransactionsReport (reportopts_ $ cliopts_ opts) j m

  defaultLayout $ do
    setTitle "journal - hledger-web"
    $(widgetFile "journal")
