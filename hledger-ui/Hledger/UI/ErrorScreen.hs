-- The error screen, showing a current error condition (such as a parse error after reloading the journal)

{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Hledger.UI.ErrorScreen
 (screen)
where

-- import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
-- import Data.Maybe
import Data.Time.Calendar (Day)
import Graphics.Vty as Vty
import Brick
-- import Brick.Widgets.List
-- import Brick.Widgets.Border
-- import Brick.Widgets.Border.Style
-- import Brick.Widgets.Center
-- import Text.Printf

-- import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.UIOptions
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIUtils

screen = ErrorScreen{
   esState  = ""
  ,sInitFn    = initErrorScreen
  ,sDrawFn    = drawErrorScreen
  ,sHandleFn = handleErrorScreen
  }

initErrorScreen :: Day -> AppState -> AppState
initErrorScreen _ st@AppState{aScreen=ErrorScreen{}} = st
initErrorScreen _ _ = error "init function called with wrong screen type, should not happen"

drawErrorScreen :: AppState -> [Widget]
drawErrorScreen AppState{ -- aopts=_uopts@UIOpts{cliopts_=_copts@CliOpts{reportopts_=_ropts@ReportOpts{query_=querystr}}},
                             aScreen=ErrorScreen{esState=err}} = [ui]
  where
    toplabel = withAttr ("border" <> "bold") (str "Oops. Please fix this problem then press g to reload")
            -- <+> str " transactions"
            -- <+> borderQueryStr querystr -- no, account transactions report shows all transactions in the acct ?
            -- <+> str " and subs"
    --         <+> str " ("
    --         <+> cur
    --         <+> str "/"
    --         <+> total
    --         <+> str ")"
    -- cur = str $ case l^.listSelectedL of
    --              Nothing -> "-"
    --              Just i -> show (i + 1)
    -- total = str $ show $ length displayitems
    -- displayitems = V.toList $ l^.listElementsL
    bottomlabel = borderKeysStr [
       -- ("up/down/pgup/pgdown/home/end", "move")
       ("g", "reload")
      -- ,("left", "return to accounts")
      ]


    -- query = query_ $ reportopts_ $ cliopts_ opts

    ui = Widget Greedy Greedy $ do

      -- calculate column widths, based on current available width
      -- c <- getContext
      -- let
      --   totalwidth = c^.availWidthL
      --                - 2 -- XXX due to margin ? shouldn't be necessary (cf UIUtils)

      render $ defaultLayout toplabel bottomlabel $ withAttr "error" $ str err

drawErrorScreen _ = error "draw function called with wrong screen type, should not happen"

-- drawErrorItem :: (Int,Int,Int,Int,Int) -> Bool -> (String,String,String,String,String) -> Widget
-- drawErrorItem (datewidth,descwidth,acctswidth,changewidth,balwidth) selected (date,desc,accts,change,bal) =
--   Widget Greedy Fixed $ do
--     render $
--       str (fitString (Just datewidth) (Just datewidth) True True date) <+>
--       str "  " <+>
--       str (fitString (Just descwidth) (Just descwidth) True True desc) <+>
--       str "  " <+>
--       str (fitString (Just acctswidth) (Just acctswidth) True True accts) <+>
--       str "   " <+>
--       withAttr changeattr (str (fitString (Just changewidth) (Just changewidth) True False change)) <+>
--       str "   " <+>
--       withAttr balattr (str (fitString (Just balwidth) (Just balwidth) True False bal))
--   where
--     changeattr | '-' `elem` change = sel $ "list" <> "amount" <> "decrease"
--                | otherwise         = sel $ "list" <> "amount" <> "increase"
--     balattr    | '-' `elem` bal    = sel $ "list" <> "balance" <> "negative"
--                | otherwise         = sel $ "list" <> "balance" <> "positive"
--     sel | selected  = (<> "selected")
--         | otherwise = id

handleErrorScreen :: AppState -> Vty.Event -> EventM (Next AppState)
handleErrorScreen st@AppState{
   aScreen=s@ErrorScreen{esState=_err}
  ,aopts=UIOpts{cliopts_=_copts}
  ,ajournal=j
  } e = do
  -- d <- liftIO getCurrentDay
  -- let
  --   reload = continue . initErrorScreen d
  case e of
    Vty.EvKey Vty.KEsc []        -> halt st
    Vty.EvKey (Vty.KChar 'q') [] -> halt st

    Vty.EvKey (Vty.KChar 'g') [] -> do
      ej <- liftIO $ journalReload j
      case ej of
        Left err -> continue st{aScreen=s{esState=err}} -- show latest parse error
        Right _j' -> continue $ popScreen st  -- return to previous screen. XXX should reload it too

      -- (ej, changed) <- liftIO $ journalReloadIfChanged copts j
      -- case (changed, ej) of
      --   (True, Right j') -> reload st{ajournal=j'}
      --   -- (True, Left err) -> continue st{amsg=err} -- XXX report parse error
      --   _                -> continue st

    -- Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st
    -- Vty.EvKey (Vty.KRight) []    -> error (show curItem) where curItem = listSelectedElement is
    -- fall through to the list's event handler (handles [pg]up/down)
    _                       -> do continue st
                                 -- is' <- handleEvent ev is
                                 -- continue st{aScreen=s{rsState=is'}}
                                 -- continue =<< handleEventLensed st someLens e
handleErrorScreen _ _ = error "event handler called with wrong screen type, should not happen"

