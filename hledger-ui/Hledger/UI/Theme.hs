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
)
where

import qualified Data.Map as M
import Data.Maybe
import Graphics.Vty
import Brick

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
active = fg brightWhite & bold
selectbg = yellow
select = black `on` selectbg

themesList :: [(String, AttrMap)]
themesList = [
   ("default", attrMap (black `on` white) [
     ("border"                                        , white `on` black & dim)
    ,("border" <> "bold"                              , currentAttr & bold)
    ,("border" <> "depth"                             , active)
    ,("border" <> "filename"                          , currentAttr)
    ,("border" <> "key"                               , active)
    ,("border" <> "minibuffer"                        , white `on` black & bold)
    ,("border" <> "query"                             , active)
    ,("border" <> "selected"                          , active)
    ,("error"                                         , fg red)
    ,("help"                                          , white `on` black & dim)
    ,("help" <> "heading"                             , fg yellow)
    ,("help" <> "key"                                 , active)
    -- ,("list"                                          , black `on` white)
    -- ,("list" <> "amount"                              , currentAttr)
    ,("list" <> "amount" <> "decrease"                , fg red)
    -- ,("list" <> "amount" <> "increase"                , fg green)
    ,("list" <> "amount" <> "decrease" <> "selected"  , red `on` selectbg & bold)
    -- ,("list" <> "amount" <> "increase" <> "selected"  , green `on` selectbg & bold)
    ,("list" <> "balance"                             , currentAttr & bold)
    ,("list" <> "balance" <> "negative"               , fg red)
    ,("list" <> "balance" <> "positive"               , fg black)
    ,("list" <> "balance" <> "negative" <> "selected" , red `on` selectbg & bold)
    ,("list" <> "balance" <> "positive" <> "selected" , select & bold)
    ,("list" <> "selected"                            , select)
    -- ,("list" <> "accounts"                         , white `on` brightGreen)
    -- ,("list" <> "selected"                         , black `on` brightYellow)
  ])

  ,("greenterm", attrMap (green `on` black) [
    ("list" <> "selected"                             , black `on` green)
  ])

  ,("terminal", attrMap defAttr [
    ("border"                                         , white `on` black),
    ("list"                                           , defAttr),
    ("list" <> "selected"                             , defAttr & reverseVideo)
  ])

  ]

-- halfbrightattr = defAttr & dim
-- reverseattr = defAttr & reverseVideo
-- redattr = defAttr `withForeColor` red
-- greenattr = defAttr `withForeColor` green
-- reverseredattr = defAttr & reverseVideo `withForeColor` red
-- reversegreenattr= defAttr & reverseVideo `withForeColor` green
