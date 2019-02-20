{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Web.Handler.AddR
  ( getAddR
  , postAddR
  ) where

import qualified Data.Text as T
import Text.Blaze.Html (preEscapedToHtml)

import Hledger
import Hledger.Cli.Commands.Add (appendToJournalFileOrStdout)
import Hledger.Web.Import
import Hledger.Web.Widget.AddForm (addForm)

getAddR :: Handler ()
getAddR = postAddR

postAddR :: Handler ()
postAddR = do
  VD{caps, j, today} <- getViewData
  when (CapAdd `notElem` caps) (permissionDenied "Missing the 'add' capability")

  ((res, view), enctype) <- runFormPost $ addForm j today
  case res of
    FormSuccess res' -> do
      let t = txnTieKnot res'
      -- XXX(?) move into balanceTransaction
      liftIO $ ensureJournalFileExists (journalFilePath j)
      liftIO $ appendToJournalFileOrStdout (journalFilePath j) (showTransaction t)
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
          <form#addform.form.col-xs-12.col-md-8 method=post enctype=#{enctype}>
            ^{view}
      |]
