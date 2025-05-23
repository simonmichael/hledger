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
  , getOpenApiR
  ) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
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
  VD{j} <- getViewData
  require EditPermission
  defaultLayout $ do
    setTitle "Edit journal"
    $(widgetFile "manage")

getDownloadR :: FilePath -> Handler TypedContent
getDownloadR f = do
  checkServerSideUiEnabled
  VD{j} <- getViewData
  require EditPermission
  (f', txt) <- journalFile404 f j
  addHeader "Content-Disposition" ("attachment; filename=\"" <> T.pack f' <> "\"")
  sendResponse ("text/plain" :: ByteString, toContent txt)

-- hledger-web equivalents of the old hledger-api's handlers

getVersionR :: Handler TypedContent
getVersionR = do
  require ViewPermission
  selectRep $ provideJson $ packageversion

getAccountnamesR :: Handler TypedContent
getAccountnamesR = do
  VD{j} <- getViewData
  require ViewPermission
  selectRep $ provideJson $ journalAccountNames j

getTransactionsR :: Handler TypedContent
getTransactionsR = do
  VD{j} <- getViewData
  require ViewPermission
  selectRep $ provideJson $ jtxns j

getPricesR :: Handler TypedContent
getPricesR = do
  VD{j} <- getViewData
  require ViewPermission
  selectRep $
    provideJson $ map priceDirectiveToMarketPrice $ jpricedirectives j

getCommoditiesR :: Handler TypedContent
getCommoditiesR = do
  VD{j} <- getViewData
  require ViewPermission
  selectRep $ do
    provideJson $ (M.keys . jinferredcommoditystyles) j

getAccountsR :: Handler TypedContent
getAccountsR = do
  VD{j} <- getViewData
  require ViewPermission
  selectRep $ do
    provideJson $
      styleAmounts (journalCommodityStylesWith HardRounding j) $
      flattenAccounts $ mapAccounts (accountSetDeclarationInfo j) $ ledgerRootAccount $ ledgerFromJournal Any j

getAccounttransactionsR :: Text -> Handler TypedContent
getAccounttransactionsR a = do
  VD{j} <- getViewData
  require ViewPermission
  let
    rspec = defreportspec
    thisacctq = Acct $ accountNameToAccountRegex a -- includes subs
  selectRep $ do
    provideJson $
      styleAmounts (journalCommodityStylesWith HardRounding j) $
      accountTransactionsReport rspec{_rsQuery=Any} j thisacctq

openApiYaml :: BS.ByteString
openApiYaml = $(embedFile "config/openapi.yaml")

getOpenApiR :: Handler Value
getOpenApiR =
  case Yaml.decodeEither' openApiYaml of
    Left _ -> notFound
    Right openapi -> do
      addHeader "Content-Type" "application/json"
      return openapi
