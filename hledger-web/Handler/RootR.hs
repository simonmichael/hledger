-- | Site root and misc. handlers.

module Handler.RootR where

import Import

getRootR :: Handler RepHtml
getRootR = redirect defaultroute where defaultroute = RegisterR
