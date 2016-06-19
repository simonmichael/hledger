{- |
Overview:
hledger-ui's UIState holds the currently active screen and any previously visited
screens (and their states).
The brick App delegates all event-handling and rendering
to the UIState's active screen.
Screens have their own screen state, render function, event handler, and app state
update function, so they have full control.

@
Brick.defaultMain brickapp st
  where
    brickapp :: App (UIState) V.Event
    brickapp = App {
        appLiftVtyEvent = id
      , appStartEvent   = return
      , appAttrMap      = const theme
      , appChooseCursor = showFirstCursor
      , appHandleEvent  = \st ev -> sHandle (aScreen st) st ev
      , appDraw         = \st    -> sDraw   (aScreen st) st
      }
    st :: UIState
    st = sInit d
         UIState{
            aopts=uopts'
           ,ajournal=j
           ,aScreen=s
           ,aPrevScreens=prevscrs
           ,aMinibuffer=Nothing
           }
@
-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Hledger.UI.UITypes where

import Data.Monoid
import Data.Time.Calendar (Day)
import Graphics.Vty
import Brick
import Brick.Widgets.List
import Brick.Widgets.Edit (Editor)
import qualified Data.Vector as V
import Lens.Micro.Platform
import Text.Show.Functions ()
  -- import the Show instance for functions. Warning, this also re-exports it

import Hledger
import Hledger.UI.UIOptions

instance Show (List a) where show _ = "<List>"
instance Show Editor   where show _ = "<Editor>"

-- | hledger-ui's application state. This holds one or more stateful screens.
-- As you navigate through screens, the old ones are saved in a stack.
-- The app can be in one of several modes: normal screen operation,
-- showing a help dialog, entering data in the minibuffer etc.
data UIState = UIState {
   aopts        :: UIOpts    -- ^ the command-line options and query arguments currently in effect
  ,ajournal     :: Journal   -- ^ the journal being viewed
  ,aPrevScreens :: [Screen]  -- ^ previously visited screens, most recent first
  ,aScreen      :: Screen    -- ^ the currently active screen
  ,aMode        :: Mode      -- ^ the currently active mode
  } deriving (Show)

-- | The mode modifies the screen's rendering and event handling.
-- It resets to Normal when entering a new screen.
data Mode =
    Normal
  | Help
  | Minibuffer Editor
  deriving (Show,Eq)

-- Ignore the editor when comparing Modes.
instance Eq Editor where _ == _ = True


-- | hledger-ui screen types & instances.
-- Each screen type has generically named initialisation, draw, and event handling functions,
-- and zero or more uniquely named screen state fields, which hold the data for a particular
-- instance of this screen.
data Screen
  = AcctsScreen AccountsScreen
  | RegScreen   RegisterScreen
  | TransScreen TransactionScreen
  | ErrScreen   ErrorScreen
  deriving (Show)

-- | function to initialise or update this screen's state
sInit :: Day -> Bool -> UIState -> UIState
sInit day reset state =
  case aScreen state of
    AcctsScreen s -> asInit s day reset state
    RegScreen   s -> rsInit s day reset state
    TransScreen s -> tsInit s day reset state
    ErrScreen   s -> esInit s day reset state

-- | brick renderer for this screen
sDraw :: UIState -> [Widget]
sDraw state =
  case aScreen state of
    AcctsScreen s -> asDraw s state
    RegScreen   s -> rsDraw s state
    TransScreen s -> tsDraw s state
    ErrScreen   s -> esDraw s state

-- | brick event handler for this screen
sHandle :: UIState -> Event -> EventM (Next UIState)
sHandle state event =
  case aScreen state of
    AcctsScreen s -> asHandle s state event
    RegScreen   s -> rsHandle s state event
    TransScreen s -> tsHandle s state event
    ErrScreen   s -> esHandle s state event


data AccountsScreen = AccountsScreen
  { asInit   :: Day -> Bool -> UIState -> UIState
  , asDraw   :: UIState -> [Widget]
  , asHandle :: UIState -> Event -> EventM (Next UIState)
  -- state fields.These ones have lenses:
  ,_asList            :: List AccountsScreenItem  -- ^ list widget showing account names & balances
  ,_asSelectedAccount :: AccountName              -- ^ a backup of the account name from the list widget's selected item (or "")
  } deriving (Show)

data RegisterScreen = RegisterScreen
  { rsInit   :: Day -> Bool -> UIState -> UIState
  , rsDraw   :: UIState -> [Widget]
  , rsHandle :: UIState -> Event -> EventM (Next UIState)
    --
  , rsList    :: List RegisterScreenItem           -- ^ list widget showing transactions affecting this account
  , rsAccount :: AccountName                       -- ^ the account this register is for
  } deriving (Show)

data TransactionScreen = TransactionScreen
  { tsInit   :: Day -> Bool -> UIState -> UIState
  , tsDraw   :: UIState -> [Widget]
  , tsHandle :: UIState -> Event -> EventM (Next UIState)
    --
  , tsTransaction  :: NumberedTransaction          -- ^ the transaction we are currently viewing, and its position in the list
  , tsTransactions :: [NumberedTransaction]        -- ^ list of transactions we can step through
  , tsAccount      :: AccountName                  -- ^ the account whose register we entered this screen from
  } deriving (Show)

data ErrorScreen = ErrorScreen
  { esInit   :: Day -> Bool -> UIState -> UIState
  , esDraw   :: UIState -> [Widget]
  , esHandle :: UIState -> Event -> EventM (Next UIState)
    --
  , esError :: String                              -- ^ error message to show
  } deriving (Show)

-- | An item in the accounts screen's list of accounts and balances.
data AccountsScreenItem = AccountsScreenItem {
   asItemIndentLevel        :: Int          -- ^ indent level
  ,asItemAccountName        :: AccountName  -- ^ full account name
  ,asItemDisplayAccountName :: AccountName  -- ^ full or short account name to display
  ,asItemRenderedAmounts    :: [String]     -- ^ rendered amounts
  }

-- | An item in the register screen's list of transactions in the current account.
data RegisterScreenItem = RegisterScreenItem {
   rsItemDate           :: String           -- ^ date
  ,rsItemDescription    :: String           -- ^ description
  ,rsItemOtherAccounts  :: String           -- ^ other accounts
  ,rsItemChangeAmount   :: String           -- ^ the change to the current account from this transaction
  ,rsItemBalanceAmount  :: String           -- ^ the balance or running total after this transaction
  ,rsItemTransaction    :: Transaction      -- ^ the full transaction
  }

type NumberedTransaction = (Integer, Transaction)

-- dummy monoid instance needed make lenses work with List fields not common across constructors
instance Monoid (List a)
  where
    mempty        = list "" V.empty 1
    mappend l1 l2 = l1 & listElementsL .~ (l1^.listElementsL <> l2^.listElementsL)

makeLenses ''AccountsScreen
