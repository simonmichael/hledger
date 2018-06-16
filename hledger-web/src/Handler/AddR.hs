{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.AddR
  ( getAddR
  , postAddR
  ) where

import Import

import Hledger
import Hledger.Cli.Commands.Add (appendToJournalFileOrStdout)
import Widget.AddForm (addForm)
import Widget.Common (fromFormSuccess)

getAddR :: Handler ()
getAddR = postAddR

postAddR :: Handler ()
postAddR = do
  VD{j, today} <- getViewData
  ((res, view), enctype) <- runFormPost $ addForm j today
  t <- txnTieKnot <$> fromFormSuccess (showForm view enctype) res
  -- XXX(?) move into balanceTransaction
  liftIO $ ensureJournalFileExists (journalFilePath j)
  liftIO $ appendToJournalFileOrStdout (journalFilePath j) (showTransaction t)
  setMessage "Transaction added."
  redirect JournalR
  where
    showForm view enctype =
      sendResponse =<< defaultLayout [whamlet|
        <h2>Add transaction
        <div .row style="margin-top:1em">
          <form#addform.form.col-xs-12.col-md-8 method=post enctype=#{enctype}>
            ^{view}
      |]
