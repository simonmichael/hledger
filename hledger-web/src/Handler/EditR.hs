{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.EditR
  ( getEditR
  , postEditR
  ) where

import Import

import qualified Data.Text as T

import Hledger
import Hledger.Cli.Utils

editForm :: [(FilePath, Text)] -> Markup -> MForm Handler (FormResult (FilePath, Text), Widget)
editForm journals = identifyForm "import" $ \extra -> do
  let files = fst <$> journals
  (jRes, jView) <- mreq (selectFieldList ((\x -> (T.pack x, x)) <$> files)) "journal" (listToMaybe files)
  (tRes, tView) <- mreq textareaField "text" (Textarea . snd <$> listToMaybe journals)
  pure ((,) <$> jRes <*> (unTextarea <$> tRes), [whamlet|
    #{extra}
    <p>
      ^{fvInput jView}<br>
      ^{fvInput tView}
      <input type=submit value="Introduce myself">
  |])

getEditR :: Handler Html
getEditR = do
  VD {j} <- getViewData
  (view, enctype) <- generateFormPost (editForm $ jfiles j)
  defaultLayout [whamlet|<form enctype=#{enctype}>^{view}|]

postEditR :: Handler Html
postEditR = do
  VD {j} <- getViewData
  ((res, view), enctype) <- runFormPost (editForm $ jfiles j)
  case res of
    FormMissing -> defaultLayout [whamlet|<form enctype=#{enctype}>^{view}|]
    FormFailure _ -> defaultLayout [whamlet|<form enctype=#{enctype}>^{view}|]
    FormSuccess (journalPath, text) -> do
      -- try to avoid unnecessary backups or saving invalid data
      _ <- liftIO $ first T.pack <$> readJournal def (Just journalPath) text
      _ <- liftIO $ writeFileWithBackupIfChanged journalPath text
      setMessage $ toHtml (printf "Saved journal %s\n" journalPath :: String)
      redirect JournalR
