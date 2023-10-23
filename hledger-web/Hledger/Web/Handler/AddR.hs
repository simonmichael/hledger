{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Web.Handler.AddR
  ( getAddR
  , postAddR
  , putAddR
  ) where

import Data.Aeson.Types (Result(..))
import qualified Data.Text as T
import Network.HTTP.Types.Status (status400)
import Text.Blaze.Html (preEscapedToHtml)
import Yesod

import Hledger
import Hledger.Cli.Commands.Add (appendToJournalFileOrStdout, journalAddTransaction)
import Hledger.Web.Import
import Hledger.Web.WebOptions (WebOpts(..))
import Hledger.Web.Widget.AddForm (addForm)

getAddR :: Handler ()
getAddR = do
  checkServerSideUiEnabled
  postAddR

postAddR :: Handler ()
postAddR = do
  checkServerSideUiEnabled
  VD{perms, j, today} <- getViewData
  when (AddPermission `notElem` perms) (permissionDenied "Missing the 'add' permission")

  ((res, view), enctype) <- runFormPost $ addForm j today
  case res of
    FormSuccess (t,f) -> do
      let t' = txnTieKnot t
      liftIO $ do
        ensureJournalFileExists f
        appendToJournalFileOrStdout f (showTransaction t')
      setMessage "Transaction added."
      redirect JournalR
    FormMissing -> showForm view enctype
    FormFailure errs -> do
      mapM_ (setMessage . preEscapedToHtml . T.replace "\n" "<br>") errs
      showForm view enctype
  where
    showForm view enctype =
      sendResponse =<< defaultLayout [whamlet|
        <h2>Add transaction
        <div .row style="margin-top:1em">
          <form#addform.form.col-xs-12.col-sm-11 method=post enctype=#{enctype}>
            ^{view}
      |]

-- Add a single new transaction, send as JSON via PUT, to the journal.
-- The web form handler above should probably use PUT as well.
putAddR :: Handler RepJson
putAddR = do
  VD{perms, j, opts} <- getViewData
  when (AddPermission `notElem` perms) (permissionDenied "Missing the 'add' permission")

  (r :: Result Transaction) <- parseCheckJsonBody
  case r of
    Error err -> sendStatusJSON status400 ("could not parse json: " ++ err ::String)
    Success t -> do
      void $ liftIO $ journalAddTransaction j (cliopts_ opts) t
      sendResponseCreated TransactionsR
