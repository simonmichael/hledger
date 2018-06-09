{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.ImportR
  ( postImportR
  ) where

import Import

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except

import Handler.Common (showErrors)

-- | Handle a post from the journal import form.
postImportR :: Handler ()
postImportR = runE $ do
  ((res, _), _) <- lift . runFormPost . renderDivs $ areq fileField "file" Nothing
  case res of
    FormMissing -> throwE ["No file provided"]
    FormFailure es -> throwE es
    FormSuccess _ -> do
     setMessage "File uploaded successfully"
     redirect JournalR
  where
    runE :: ExceptT [Text] Handler () -> Handler ()
    runE f = runExceptT f >>= \case
      Left e -> showErrors e >> redirect JournalR
      Right x -> pure x
