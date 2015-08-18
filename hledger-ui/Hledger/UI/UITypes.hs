module Hledger.UI.UITypes where

import Data.Time.Calendar (Day)
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import Brick.Widgets.Core
  ( Widget(..)
  )

import Hledger
import Hledger.UI.Options

----------------------------------------------------------------------

-- | hledger-ui's application state. This is part of, but distinct
-- from, brick's M.App.
data AppState = AppState {
   aopts :: UIOpts          -- ^ command-line options at startup
  ,aargs :: [String]        -- ^ command-line arguments at startup
  ,ajournal :: Journal      -- ^ the parsed journal
  ,aScreen :: Screen        -- ^ the currently active screen
  ,aPrevScreens :: [Screen] -- ^ previously visited screens
  } deriving (Show)

-- | Types of screen available within the app, along with their state.
-- Screen types are distinguished by their constructor and by the type
-- of their state (hence the unique accessor names for the latter).
data Screen =
    AccountsScreen {
     asState :: L.List BalanceReportItem                            -- ^ the screen's state (data being displayed and widget state)
    ,sInitFn :: Day -> [String] -> AppState -> AppState                         -- ^ function to initialise the screen's state on entry
    ,sHandleFn :: AppState -> V.Event -> M.EventM (M.Next AppState) -- ^ brick event handler to use for this screen
    ,sDrawFn :: AppState -> [Widget]                                -- ^ brick renderer to use for this screen
    }
  | RegisterScreen {
     rsState :: L.List PostingsReportItem
    ,sInitFn :: Day -> [String] -> AppState -> AppState
    ,sHandleFn :: AppState -> V.Event -> M.EventM (M.Next AppState)
    ,sDrawFn :: AppState -> [Widget]
    }
  deriving (Show)

instance Show (L.List a) where show _ = "<List>"
