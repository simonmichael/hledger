{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Handler.UploadR
  ( getUploadR
  , postUploadR
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Conduit (connect)
import Data.Conduit.Binary (sinkLbs)
import qualified Data.Text.Encoding as TE

import Hledger.Web.Import
import Hledger.Web.Widget.Common (fromFormSuccess, journalFile404, writeJournalTextIfValidAndChanged)

uploadForm :: FilePath -> Markup -> MForm Handler (FormResult FileInfo, Widget)
uploadForm f =
  identifyForm "upload" $ \extra -> do
    (res, _) <- mreq fileField fs Nothing
    -- Ignoring the view - setting the name of the element is enough here
    pure (res, $(widgetFile "upload-form"))
  where
    fs = FieldSettings "file" Nothing (Just "file") (Just "file") []

getUploadR :: FilePath -> Handler ()
getUploadR f = do
  checkServerSideUiEnabled
  postUploadR f

postUploadR :: FilePath -> Handler ()
postUploadR f = do
  checkServerSideUiEnabled
  VD {caps, j} <- getViewData
  when (CapManage `notElem` caps) (permissionDenied "Missing the 'manage' capability")

  (f', _) <- journalFile404 f j
  ((res, view), enctype) <- runFormPost (uploadForm f')
  fi <- fromFormSuccess (showForm view enctype) res
  lbs <- BL.toStrict <$> connect (fileSource fi) sinkLbs

  -- Try to parse as UTF-8
  -- XXX Unfortunate - how to parse as system locale?
  newtxt <- case TE.decodeUtf8' lbs of
    Left e -> do
      setMessage $
        "Encoding error: '" <> toHtml (show e) <> "'. " <>
        "If your file is not UTF-8 encoded, try the 'edit form', " <>
        "where the transcoding should be handled by the browser."
      showForm view enctype
    Right newtxt -> return newtxt
  writeJournalTextIfValidAndChanged f newtxt >>= \case
    Left e -> do
      setMessage $ "Failed to load journal: " <> toHtml e
      showForm view enctype
    Right () -> do
      setMessage $ "File " <> toHtml f <> " uploaded successfully"
      redirect JournalR
  where
    showForm view enctype =
      sendResponse <=< defaultLayout $ do
        setTitle "Upload journal"
        [whamlet|<form method=post enctype=#{enctype}>^{view}|]
