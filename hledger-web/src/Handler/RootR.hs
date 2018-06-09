-- | Site root and misc. handlers.

module Handler.RootR where

import Import

getRootR :: Handler Html
getRootR = redirect JournalR
