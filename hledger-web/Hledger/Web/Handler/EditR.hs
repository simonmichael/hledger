{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Handler.EditR
  ( getEditR
  , postEditR
  ) where

import Control.Monad.Except (runExceptT)
import Hledger.Web.Import
import Hledger.Web.Widget.Common
       (fromFormSuccess, helplink, journalFile404, writeJournalTextIfValidAndChanged)

editForm :: FilePath -> Text -> Form Text
editForm f txt =
  identifyForm "edit" $ \extra -> do
    (tRes, tView) <- mreq textareaField fs (Just (Textarea txt))
    pure (unTextarea <$> tRes, $(widgetFile "edit-form"))
  where
    fs = FieldSettings "text" mzero mzero mzero [("class", "form-control"), ("rows", "25")]

getEditR :: FilePath -> Handler ()
getEditR f = do
  checkServerSideUiEnabled
  postEditR f

postEditR :: FilePath -> Handler ()
postEditR f = do
  checkServerSideUiEnabled
  VD {perms, j} <- getViewData
  when (EditPermission `notElem` perms) (permissionDenied "Missing the 'edit' permission")

  (f', txt) <- journalFile404 f j
  ((res, view), enctype) <- runFormPost (editForm f' txt)
  newtxt <- fromFormSuccess (showForm view enctype) res
  runExceptT (writeJournalTextIfValidAndChanged f newtxt) >>= \case
    Left e -> do
      setMessage $ "Failed to load journal: " <> toHtml e
      showForm view enctype
    Right () -> do
      setMessage $ "Saved journal " <> toHtml f <> "\n"
      redirect JournalR
  where
    showForm view enctype =
      sendResponse <=< defaultLayout $ do
        setTitle "Edit journal"
        [whamlet|<form method=post enctype=#{enctype}>^{view}|]
