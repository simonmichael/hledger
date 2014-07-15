-- | /journal/edit handlers.

module Handler.JournalEditR where

import Import

import Handler.Common
import Handler.Post


-- | The journal editform, no sidebar.
getJournalEditR :: Handler Html
getJournalEditR = do
  vd <- getViewData
  defaultLayout $ do
      setTitle "hledger-web journal edit form"
      toWidget $ editform vd

postJournalEditR :: Handler Html
postJournalEditR = handlePost
