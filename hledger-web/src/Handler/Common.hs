module Handler.Common
  ( getRootR
  , getFaviconR
  , getRobotsR
  ) where

import Import
import Yesod.Default.Handlers (getFaviconR, getRobotsR)

getRootR :: Handler Html
getRootR = redirect JournalR
