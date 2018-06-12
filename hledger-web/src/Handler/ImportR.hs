{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.ImportR
  ( getImportR
  , postImportR
  ) where

import Import

importForm :: Markup -> MForm Handler (FormResult FileInfo, Widget)
importForm = identifyForm "import" $ \extra -> do
  (res, view) <- mreq fileField "file" Nothing
  pure (res, [whamlet|
    #{extra}
    <p>
      Hello, my name is #
      ^{fvInput view}
      <input type=submit value="Introduce myself">
  |])

getImportR :: Handler Html
getImportR = do
  (view, enctype) <- generateFormPost importForm
  defaultLayout [whamlet|<form enctype=#{enctype}>^{view}|]

-- | Handle a post from the journal import form.
postImportR :: Handler Html
postImportR = do
  ((res, view), enctype) <- runFormPost importForm
  case res of
    FormMissing -> defaultLayout [whamlet|<form enctype=#{enctype}>^{view}|]
    FormFailure _ -> defaultLayout [whamlet|<form enctype=#{enctype}>^{view}|]
    FormSuccess _ -> do
      setMessage "File uploaded successfully"
      redirect JournalR
