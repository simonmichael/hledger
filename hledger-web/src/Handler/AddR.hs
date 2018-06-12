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

getAddR :: Handler Html
getAddR = do
  VD {j, today} <- getViewData
  (view, enctype) <- generateFormPost $ addForm j today
  defaultLayout [whamlet|<div .row><form class="addform form col-xs-12" method=post enctype=#{enctype}>^{view}|]

postAddR :: Handler Html
postAddR = do
  VD{j, today} <- getViewData
  ((res, view), enctype) <- runFormPost $ addForm j today
  case res of
    FormMissing -> defaultLayout [whamlet|<div .row><form class="addform form col-xs-12" method=post enctype=#{enctype}>^{view}|]
    FormFailure _ -> defaultLayout [whamlet|<div .row><form class="addform form col-xs-12" method=post enctype=#{enctype}>^{view}|]
    FormSuccess t -> do
      liftIO $ do
        -- XXX(?) move into balanceTransaction
        ensureJournalFileExists (journalFilePath j)
        appendToJournalFileOrStdout (journalFilePath j) (showTransaction $ txnTieKnot t)
      setMessage "Transaction added."
      redirect JournalR

