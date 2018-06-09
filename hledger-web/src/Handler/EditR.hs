{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.EditR
  ( postEditR
  ) where

import Import

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import qualified Data.Text as T
import Text.Printf (printf)

import Handler.Common (showErrors)

import Hledger
import Hledger.Cli.Utils

-- | Handle a post from the journal edit form.
postEditR :: Handler ()
postEditR = runE $ do
  VD {j} <- lift getViewData
  -- get form input values, or validation errors.
  text <- ExceptT $ maybe (Left "No value provided") Right <$> lookupPostParam "text"
  journalpath <- ExceptT $ maybe
    (Right . T.pack $ journalFilePath j)
    (\f ->
       if T.unpack f `elem` journalFilePaths j
         then Right f
         else Left "unrecognised journal file path") <$>
    lookupPostParam "journal"
  -- try to avoid unnecessary backups or saving invalid data
  let tnew = T.filter (/= '\r') text

  jE <- liftIO $ readJournal def (Just $ T.unpack journalpath) tnew
  _ <- ExceptT . pure $ first T.pack jE
  _ <- liftIO $ writeFileWithBackupIfChanged (T.unpack journalpath) tnew
  setMessage $ toHtml (printf "Saved journal %s\n" (show journalpath) :: String)
  redirect JournalR
  where
    runE :: ExceptT Text Handler () -> Handler ()
    runE f = runExceptT f >>= \case
      Left e -> showErrors [e] >> redirect JournalR
      Right x -> pure x
