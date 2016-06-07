{- |
Overview:
hledger-ui's AppState holds the active screen and any previously visited screens.
Screens have their own render state, render function, event handler,
and app state update function (which can update the whole AppState).
A brick App delegates event-handling and rendering to our AppState's active screen.

@
Brick.defaultMain brickapp st
  where
    brickapp :: App (AppState) V.Event
    brickapp = App {
        appLiftVtyEvent = id
      , appStartEvent   = return
      , appAttrMap      = const theme
      , appChooseCursor = showFirstCursor
      , appHandleEvent  = \st ev -> sHandleFn (aScreen st) st ev
      , appDraw         = \st    -> sDrawFn   (aScreen st) st
      }
    st :: AppState
    st = (sInitFn scr) d
         AppState{
            aopts=uopts'
           ,ajournal=j
           ,aScreen=scr
           ,aPrevScreens=prevscrs
           ,aMinibuffer=Nothing
           }
@
-}

{-# LANGUAGE StandaloneDeriving #-}

module Hledger.UI.UITypes where

import Data.Time.Calendar (Day)
import qualified Graphics.Vty as V
import Brick
import Brick.Widgets.List (List)
import Brick.Widgets.Edit (Editor)
import Text.Show.Functions ()
  -- import the Show instance for functions. Warning, this also re-exports it

import Hledger
import Hledger.UI.UIOptions

-- | hledger-ui's application state. This holds one or more stateful screens.
data AppState = AppState {
   aopts        :: UIOpts       -- ^ the command-line options and query arguments currently in effect
  ,ajournal     :: Journal      -- ^ the journal being viewed
  ,aScreen      :: Screen       -- ^ the currently active screen
  ,aPrevScreens :: [Screen]     -- ^ previously visited screens, most recent first
  ,aMinibuffer  :: Maybe Editor -- ^ a compact editor used for data entry, when active
  } deriving (Show)

-- | Types of screen available within hledger-ui. Each has its own
-- specific state type, and generic initialisation, event handling
-- and rendering functions.
--
-- Screen types are pattern-matched by their constructor and their
-- state field, which must have a unique name. This type causes
-- partial functions, so take care.
data Screen =
    AccountsScreen {
       asState   :: (List           --  list widget holding:
                      (Int          --   indent level
                      ,AccountName  --   full account name
                      ,AccountName  --   full or short account name to display
                      ,[String]     --   rendered amounts
                      )
                    ,AccountName    --  full name of the currently selected account (or "")
                    )
      ,sInitFn   :: Day -> AppState -> AppState                    -- ^ function to generate the screen's state on entry or change
      ,sDrawFn   :: AppState -> [Widget]                           -- ^ brick renderer to use for this screen
      ,sHandleFn :: AppState -> V.Event -> EventM (Next AppState)  -- ^ brick event handler to use for this screen
    }
  | RegisterScreen {
       rsState   :: (List           --  list widget holding:
                      (String       --   date
                      ,String       --   description
                      ,String       --   other accts
                      ,String       --   change amt
                      ,String       --   balance amt
                      ,Transaction  --   the full transaction
                      )
                    ,AccountName    --  full name of the acct we are showing a register for
                    )
      ,sInitFn   :: Day -> AppState -> AppState                    -- ^ function to generate the screen's state on entry or change
      ,sDrawFn   :: AppState -> [Widget]                           -- ^ brick renderer to use for this screen
      ,sHandleFn :: AppState -> V.Event -> EventM (Next AppState)  -- ^ brick event handler to use for this screen
    }
  | TransactionScreen {
       tsState   :: ((Integer, Transaction)    --  the (numbered) transaction we are currently viewing
                    ,[(Integer, Transaction)]  --  the list of numbered transactions we can step through
                    ,AccountName               --  the account whose register we entered this screen from
                    )
      ,sInitFn   :: Day -> AppState -> AppState
      ,sDrawFn   :: AppState -> [Widget]
      ,sHandleFn :: AppState -> V.Event -> EventM (Next AppState)
    }
  | ErrorScreen {
       esState   :: String                     --  error message to display
      ,sInitFn   :: Day -> AppState -> AppState
      ,sDrawFn   :: AppState -> [Widget]
      ,sHandleFn :: AppState -> V.Event -> EventM (Next AppState)
    }
  deriving (Show)

instance Show (List a) where show _ = "<List>"
instance Show Editor   where show _ = "<Editor>"

