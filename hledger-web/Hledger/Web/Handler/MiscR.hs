{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Handler.MiscR
  ( getAccountnamesR
  , getTransactionsR
  , getPricesR
  , getCommoditiesR
  , getAccountsR
  , getAccounttransactionsR
  , getDownloadR
  , getFaviconR
  , getManageR
  , getRobotsR
  , getRootR
  ) where

import qualified Data.Map as M
import qualified Data.Text as T
import Yesod.Default.Handlers (getFaviconR, getRobotsR)

import Hledger
import Hledger.Web.Json ()
import Hledger.Web.Import
import Hledger.Web.Widget.Common (journalFile404)

getRootR :: Handler Html
getRootR = redirect JournalR

getManageR :: Handler Html
getManageR = do
  VD{caps, j} <- getViewData
  when (CapManage `notElem` caps) (permissionDenied "Missing the 'manage' capability")
  defaultLayout $ do
    setTitle "Manage journal"
    $(widgetFile "manage")

getDownloadR :: FilePath -> Handler TypedContent
getDownloadR f = do
  VD{caps, j} <- getViewData
  when (CapManage `notElem` caps) (permissionDenied "Missing the 'manage' capability")
  (f', txt) <- journalFile404 f j
  addHeader "Content-Disposition" ("attachment; filename=\"" <> T.pack f' <> "\"")
  sendResponse ("text/plain" :: ByteString, toContent txt)

-- hledger-web equivalents of hledger-api's handlers

getAccountnamesR :: Handler TypedContent
getAccountnamesR = do
  VD{caps, j} <- getViewData
  when (CapView `notElem` caps) (permissionDenied "Missing the 'view' capability")
  selectRep $ do
    provideJson $ journalAccountNames j

getTransactionsR :: Handler TypedContent
getTransactionsR = do
  VD{caps, j} <- getViewData
  when (CapView `notElem` caps) (permissionDenied "Missing the 'view' capability")
  selectRep $ do
    provideJson $ jtxns j

getPricesR :: Handler TypedContent
getPricesR = do
  VD{caps, j} <- getViewData
  when (CapView `notElem` caps) (permissionDenied "Missing the 'view' capability")
  selectRep $ do
    provideJson $ map priceDirectiveToMarketPrice $ jpricedirectives j

getCommoditiesR :: Handler TypedContent
getCommoditiesR = do
  VD{caps, j} <- getViewData
  when (CapView `notElem` caps) (permissionDenied "Missing the 'view' capability")
  selectRep $ do
    provideJson $ (M.keys . jinferredcommodities) j

getAccountsR :: Handler TypedContent
getAccountsR = do
  VD{caps, j} <- getViewData
  when (CapView `notElem` caps) (permissionDenied "Missing the 'view' capability")
  selectRep $ do
    provideJson $ laccounts $ ledgerFromJournal Any j

getAccounttransactionsR :: Text -> Handler TypedContent
getAccounttransactionsR a = do
  VD{caps, j} <- getViewData
  when (CapView `notElem` caps) (permissionDenied "Missing the 'view' capability")
  let
    ropts = defreportopts
    q = Any --filterQuery (not . queryIsDepth) $ queryFromOpts d ropts'
    thisacctq = Acct $ accountNameToAccountRegex a -- includes subs
  selectRep $ do
    provideJson $ accountTransactionsReport ropts j q thisacctq

