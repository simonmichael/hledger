module Hledger.UI.UITypes where

import Data.Time.Calendar (Day)
import qualified Graphics.Vty as V
import Brick
import Brick.Widgets.List (List)

import Hledger
import Hledger.UI.UIOptions

----------------------------------------------------------------------

-- | hledger-ui's application state. This is part of, but distinct
-- from, brick's App.
data AppState = AppState {
   aopts :: UIOpts          -- ^ command-line options, query, depth etc. currently in effect.
  -- ,aargs :: [String]        -- ^ command-line arguments at startup
  ,ajournal :: Journal      -- ^ the parsed journal
  ,aScreen :: Screen        -- ^ the currently active screen
  ,aPrevScreens :: [Screen] -- ^ previously visited screens
  } deriving (Show)

-- | Types of screen available within the app, along with their state.
-- Screen types are distinguished by their constructor and by the type
-- of their state (which must have unique accessor names).
data Screen =
    AccountsScreen {
     asState :: List (Int,String,String,[String])  -- ^ list of (indent level, full account name, full or short account name to display, rendered amounts)
    ,sInitFn :: Day -> AppState -> AppState                         -- ^ function to initialise the screen's state on entry
    ,sHandleFn :: AppState -> V.Event -> EventM (Next AppState) -- ^ brick event handler to use for this screen
    ,sDrawFn :: AppState -> [Widget]                                -- ^ brick renderer to use for this screen
    }
  | RegisterScreen {
     rsState :: List (String,String,String,String,String) -- ^ list of (date, description, other accts, change amt, balance amt)
    ,rsAcct :: AccountName              -- ^ the account we are showing a register for
    ,sInitFn :: Day -> AppState -> AppState
    ,sHandleFn :: AppState -> V.Event -> EventM (Next AppState)
    ,sDrawFn :: AppState -> [Widget]
    }
  | ErrorScreen {
     esState :: String -- ^ error message to display
    ,sInitFn :: Day -> AppState -> AppState
    ,sHandleFn :: AppState -> V.Event -> EventM (Next AppState)
    ,sDrawFn :: AppState -> [Widget]
    }
  deriving (Show)

instance Show (List a) where show _ = "<List>"
