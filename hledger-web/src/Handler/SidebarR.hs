-- | /sidebar

module Handler.SidebarR where

import Import

import Handler.Common (sidebar)

-- | Render just the accounts sidebar, useful when opening the sidebar.
getSidebarR :: Handler Html
getSidebarR = withUrlRenderer . sidebar =<< getViewData
