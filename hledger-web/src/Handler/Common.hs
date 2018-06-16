{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Common
  ( getDownloadR
  , getFaviconR
  , getManageR
  , getRobotsR
  , getRootR
  ) where

import Import

import qualified Data.Text as T
import Yesod.Default.Handlers (getFaviconR, getRobotsR)

import Hledger (jfiles)
import Widget.Common (journalFile404)

getRootR :: Handler Html
getRootR = redirect JournalR

getManageR :: Handler Html
getManageR = do
  VD{j} <- getViewData
  defaultLayout $ do
    setTitle "Manage journal"
    $(widgetFile "manage")

getDownloadR :: FilePath -> Handler TypedContent
getDownloadR f = do
  (f', txt) <- journalFile404 f . j =<< getViewData
  addHeader "Content-Disposition" ("attachment; filename=\"" <> T.pack f' <> "\"")
  sendResponse ("text/plain" :: ByteString, toContent txt)
