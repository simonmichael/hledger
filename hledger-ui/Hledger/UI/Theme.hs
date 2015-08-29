-- | The all-important theming engine!
--
-- Cf
-- https://hackage.haskell.org/package/vty/docs/Graphics-Vty-Attributes.html
-- http://hackage.haskell.org/package/brick/docs/Brick-AttrMap.html
-- http://hackage.haskell.org/package/brick-0.1/docs/Brick-Util.html
-- http://hackage.haskell.org/package/brick-0.1/docs/Brick-Widgets-Core.html#g:5
-- http://hackage.haskell.org/package/brick-0.1/docs/Brick-Widgets-Border.html


{-# LANGUAGE OverloadedStrings #-}

module Hledger.UI.Theme (
   defaultTheme
  ,getTheme
  ,themes
  ,themeNames
 ) where

import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Graphics.Vty
import Brick
import Brick.Widgets.Border
import Brick.Widgets.List

defaultTheme :: AttrMap
defaultTheme = fromMaybe (snd $ head themesList) $ getTheme "white"
  -- the theme named here should exist;
  -- otherwise it will take the first one from the list,
  -- which must be non-empty.

-- | Look up the named theme, if it exists.
getTheme :: String -> Maybe AttrMap
getTheme name = M.lookup name themes

-- | A selection of named themes specifying terminal colours and styles.
-- One of these is active at a time.
--
-- A hledger-ui theme is a vty/brick AttrMap.  Each theme specifies a
-- default style (Attr), plus extra styles which are applied when
-- their (hierarchical) name matches the widget rendering context.
-- "More specific styles, if present, are used and only fall back to
-- more general ones when the more specific ones are absent, but also
-- these styles get merged, so that if a more specific style only
-- provides the foreground color, its more general parent style can
-- set the background color, too."
-- For example: rendering a widget named "b" inside a widget named "a",
-- - if a style named "a" <> "b" exists, it will be used. Anything it
--   does not specify will be taken from a style named "a" if that
--   exists, otherwise from the default style.
-- - otherwise if a style named "a" exists, it will be used, and
--   anything it does not specify will be taken from the default style.
-- - otherwise (you guessed it) the default style is used.
--
themes :: M.Map String AttrMap
themes = M.fromList themesList

themeNames :: [String]
themeNames = map fst themesList

(&) = withStyle

themesList :: [(String, AttrMap)]
themesList = [
  ("default", attrMap
            (black `on` white & bold) [ -- default style for this theme
              (borderAttr       , white `on` black & dim),
              (borderAttr <> "bold", white `on` black & bold),
              (borderAttr <> "query", yellow `on` black & bold),
              (borderAttr <> "depth", cyan `on` black & bold),
              -- ("normal"                , black `on` white),
              ("list"                  , black `on` white),      -- regular list items
              ("list" <> "selected"    , white `on` blue & bold), -- selected list items
              -- ("list" <> "selected"     , black `on` brightYellow),
              -- ("list" <> "accounts"  , white `on` brightGreen),
              ("list" <> "amount" <> "increase", currentAttr `withForeColor` green),
              ("list" <> "amount" <> "decrease", currentAttr `withForeColor` red),
              ("list" <> "balance" <> "positive",  currentAttr `withForeColor` black),
              ("list" <> "balance" <> "negative", currentAttr `withForeColor` red),
              ("list" <> "amount" <> "increase" <> "selected", brightGreen `on` blue & bold),
              ("list" <> "amount" <> "decrease" <> "selected", brightRed `on` blue & bold),
              ("list" <> "balance" <> "positive" <> "selected",  white `on` blue & bold),
              ("list" <> "balance" <> "negative" <> "selected", brightRed `on` blue & bold)
              ]),

  ("terminal", attrMap
            defAttr [  -- use the current terminal's default style
              (borderAttr       , white `on` black),
              -- ("normal"         , defAttr),
              (listAttr         , defAttr),
              (listSelectedAttr , defAttr & reverseVideo & bold)
              -- ("status"         , defAttr & reverseVideo)
              ]),

  ("greenterm", attrMap
            (green `on` black) [
              -- (listAttr                  , green `on` black),
              (listSelectedAttr          , black `on` green & bold)
              ])
  -- ("colorful", attrMap
  --           defAttr [
  --             (listAttr         , defAttr & reverseVideo),
  --             (listSelectedAttr , defAttr `withForeColor` white `withBackColor` red)
  --             -- ("status"         , defAttr `withForeColor` black `withBackColor` green)
  --             ])

  ]

-- halfbrightattr = defAttr & dim
-- reverseattr = defAttr & reverseVideo
-- redattr = defAttr `withForeColor` red
-- greenattr = defAttr `withForeColor` green
-- reverseredattr = defAttr & reverseVideo `withForeColor` red
-- reversegreenattr= defAttr & reverseVideo `withForeColor` green

