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

import Data.IORef (writeIORef)

import Hledger.Web.Import
import Hledger.Web.Widget.Common
       (fromFormSuccess, helplink, journalFile404, writeValidJournal)

editForm :: FilePath -> Text -> Markup -> MForm Handler (FormResult Text, Widget)
editForm f txt =
  identifyForm "edit" $ \extra -> do
    (tRes, tView) <- mreq textareaField fs (Just (Textarea txt))
    pure (unTextarea <$> tRes, $(widgetFile "edit-form"))
  where
    fs = FieldSettings "text" mzero mzero mzero [("class", "form-control"), ("rows", "25")]

getEditR :: FilePath -> Handler ()
getEditR = postEditR

postEditR :: FilePath -> Handler ()
postEditR f = do
  VD {caps, j} <- getViewData
  App { appJournal } <- getYesod
  when (CapManage `notElem` caps) (permissionDenied "Missing the 'manage' capability")

  (f', txt) <- journalFile404 f j
  ((res, view), enctype) <- runFormPost (editForm f' txt)
  text <- fromFormSuccess (showForm view enctype) res
  writeValidJournal f text >>= \case
    Left e -> do
      setMessage $ "Failed to load journal: " <> toHtml e
      showForm view enctype
    Right j' -> do
      -- explicitly write to IORef for journals read from stdin
      liftIO $ writeIORef appJournal j'
      setMessage $ "Saved journal " <> toHtml f <> "\n"
      redirect JournalR
  where
    showForm view enctype =
      sendResponse <=< defaultLayout $ do
        setTitle "Edit journal"
        [whamlet|<form method=post enctype=#{enctype}>^{view}|]
