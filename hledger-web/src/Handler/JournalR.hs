-- | /journal handlers.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.JournalR where

import Import

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Web.WebOptions
import Widget.AddForm (addForm)
import Widget.Common (accountQuery, mixedAmountAsHtml)

-- | The formatted journal view, with sidebar.
-- XXX like registerReportAsHtml
getJournalR :: Handler Html
getJournalR = do
  VD{j, m, opts, qopts, today} <- getViewData
  let title = case inAccount qopts of
        Nothing -> "General Journal"
        Just (a, inclsubs) -> "Transactions in " <> a <> if inclsubs then "" else " (excluding subaccounts)"
      title' = title <> if m /= Any then ", filtered" else ""
      acctlink a = (RegisterR, [("q", accountQuery a)])
      (_, items) = journalTransactionsReport (reportopts_ $ cliopts_ opts) j m

  (addView, addEnctype) <- generateFormPost (addForm j today)
  defaultLayout $ do
    setTitle "journal - hledger-web"
    $(widgetFile "journal")
