{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Handler.MiscR
  ( getVersionR
  , getAccountnamesR
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
import Hledger.Web.Import
import Hledger.Web.WebOptions (packageversion)
import Hledger.Web.Widget.Common (journalFile404)

getRootR :: Handler Html
getRootR = do
  checkServerSideUiEnabled
  redirect JournalR

getManageR :: Handler Html
getManageR = do
  checkServerSideUiEnabled
  VD{perms, j} <- getViewData
  when (EditPermission `notElem` perms) (permissionDenied "Missing the 'edit' permission")
  defaultLayout $ do
    setTitle "Edit journal"
    $(widgetFile "manage")

getDownloadR :: FilePath -> Handler TypedContent
getDownloadR f = do
  checkServerSideUiEnabled
  VD{perms, j} <- getViewData
  when (EditPermission `notElem` perms) (permissionDenied "Missing the 'edit' permission")
  (f', txt) <- journalFile404 f j
  addHeader "Content-Disposition" ("attachment; filename=\"" <> T.pack f' <> "\"")
  sendResponse ("text/plain" :: ByteString, toContent txt)

-- hledger-web equivalents of the old hledger-api's handlers

getVersionR :: Handler TypedContent
getVersionR = do
  VD{perms} <- getViewData
  when (ViewPermission `notElem` perms) (permissionDenied "Missing the 'view' permission")
  selectRep $ do
    provideJson $ packageversion

getAccountnamesR :: Handler TypedContent
getAccountnamesR = do
  VD{perms, j} <- getViewData
  when (ViewPermission `notElem` perms) (permissionDenied "Missing the 'view' permission")
  selectRep $ do
    provideJson $ journalAccountNames j

getTransactionsR :: Handler TypedContent
getTransactionsR = do
  VD{perms, j} <- getViewData
  when (ViewPermission `notElem` perms) (permissionDenied "Missing the 'view' permission")
  selectRep $ do
    provideJson $ jtxns j

getPricesR :: Handler TypedContent
getPricesR = do
  VD{perms, j} <- getViewData
  when (ViewPermission `notElem` perms) (permissionDenied "Missing the 'view' permission")
  selectRep $ do
    provideJson $ map priceDirectiveToMarketPrice $ jpricedirectives j

getCommoditiesR :: Handler TypedContent
getCommoditiesR = do
  VD{perms, j} <- getViewData
  when (ViewPermission `notElem` perms) (permissionDenied "Missing the 'view' permission")
  selectRep $ do
    provideJson $ (M.keys . jinferredcommodities) j

getAccountsR :: Handler TypedContent
getAccountsR = do
  VD{perms, j} <- getViewData
  when (ViewPermission `notElem` perms) (permissionDenied "Missing the 'view' permission")
  selectRep $ do
    provideJson $ flattenAccounts $ mapAccounts (accountSetDeclarationInfo j) $ ledgerRootAccount $ ledgerFromJournal Any j

getAccounttransactionsR :: Text -> Handler TypedContent
getAccounttransactionsR a = do
  VD{perms, j} <- getViewData
  when (ViewPermission `notElem` perms) (permissionDenied "Missing the 'view' permission")
  let
    rspec = defreportspec
    thisacctq = Acct $ accountNameToAccountRegex a -- includes subs
  selectRep $ do
    provideJson $ accountTransactionsReport rspec{_rsQuery=Any} j thisacctq

