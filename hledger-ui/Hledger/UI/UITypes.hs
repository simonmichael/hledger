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
   aopts :: UIOpts          -- ^ the command-line options and query currently in effect
  ,ajournal :: Journal      -- ^ the journal being viewed
  ,aScreen :: Screen        -- ^ the currently active screen
  ,aPrevScreens :: [Screen] -- ^ previously visited screens, most recent first
  } deriving (Show)

-- | Types of screen available within the app, along with their state.
-- Screen types are distinguished by their constructor and their state
-- field, which must have unique names.
--
-- This type causes partial functions, so take care.
data Screen =
    AccountsScreen {
     asState :: (List (Int,String,String,[String]), AccountName)  -- ^ list widget holding (indent level, full account name, full or short account name to display, rendered amounts);
                                                                  --   the full name of the currently selected account (or "")
    ,sInitFn :: Day -> AppState -> AppState                       -- ^ function to initialise the screen's state on entry
    ,sHandleFn :: AppState -> V.Event -> EventM (Next AppState)   -- ^ brick event handler to use for this screen
    ,sDrawFn :: AppState -> [Widget]                              -- ^ brick renderer to use for this screen
    }
  | RegisterScreen {
     rsState :: (List (String,String,String,String,String,Transaction), AccountName)
                                                                  -- ^ list widget holding (date, description, other accts, change amt, balance amt, and the full transaction);
                                                                  --   the full name of the account we are showing a register for
    ,sInitFn :: Day -> AppState -> AppState
    ,sHandleFn :: AppState -> V.Event -> EventM (Next AppState)
    ,sDrawFn :: AppState -> [Widget]
    }
  | TransactionScreen {
     tsState :: ((Integer,Transaction), [(Integer,Transaction)], AccountName)
                -- ^ the (numbered) transaction we are currently viewing,
                --   the list of numbered transactions we can step through,
                --   and the account whose register we entered this screen from
    ,sInitFn :: Day -> AppState -> AppState
    ,sHandleFn :: AppState -> V.Event -> EventM (Next AppState)
    ,sDrawFn :: AppState -> [Widget]
    }
  | ErrorScreen {
     esState :: String                                            -- ^ error message to display
    ,sInitFn :: Day -> AppState -> AppState
    ,sHandleFn :: AppState -> V.Event -> EventM (Next AppState)
    ,sDrawFn :: AppState -> [Widget]
    }
  deriving (Show)

instance Show (List a) where show _ = "<List>"
