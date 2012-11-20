-- | /journal/edit handlers.

module Handler.JournalEditR where

import Import

import Handler.Common
import Handler.Post
import Handler.Utils


-- | The journal editform, no sidebar.
getJournalEditR :: Handler RepHtml
getJournalEditR = do
  vd <- getViewData
  defaultLayout $ do
      setTitle "hledger-web journal edit form"
      toWidget $ editform vd

postJournalEditR :: Handler RepHtml
postJournalEditR = handlePost
