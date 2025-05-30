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
import Safe (headErr)

defaultTheme :: AttrMap
defaultTheme = fromMaybe (snd $ headErr themesList) $ getTheme "white"  -- PARTIAL headErr succeeds because themesList is non-null
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

-- colors from the 8-bit (ish) 240-colour palette (see
-- <https://rich.readthedocs.io/en/stable/appendix/colors.html>, subtract
-- 16 to correct the offset from the 4-bit color palette). These colors
-- should appear the same in all supported terminals.
white8    = Color240 (253 - 16)
black8    = Color240 (232 - 16)
red8      = Color240 (124 - 16)
yellow8   = Color240 (178 - 16)
blue8     = Color240 (25 - 16)
active8   = fg (Color240 (255 - 16)) & bold
-- we use reverseVideo here to have a legible fallback if the terminal does
-- not support the colors (and falls back to black/white)
select8   = yellow8 `on` black8 & reverseVideo
selectNeg = yellow8 `on` red8 & reverseVideo -- & bold

themesList :: [(String, AttrMap)]
themesList = [
   -- the default theme, "light", defined in terms of the 240-color palette
   ("light", attrMap (black8 `on` white8) [
      (attrName "border"                                                                   , white8 `on` black8 & dim)
    , (attrName "border" <> attrName "bold"                                                , currentAttr & bold)
    , (attrName "border" <> attrName "depth"                                               , active8)
    , (attrName "border" <> attrName "filename"                                            , currentAttr)
    , (attrName "border" <> attrName "key"                                                 , active8)
    , (attrName "border" <> attrName "minibuffer"                                          , white8 `on` black8 & bold)
    , (attrName "border" <> attrName "query"                                               , active8)
    , (attrName "border" <> attrName "selected"                                            , active8)
    , (attrName "error"                                                                    , fg red8)
    , (attrName "help"                                                                     , white8 `on` black8 & dim)
    , (attrName "help" <> attrName "heading"                                               , fg yellow8)
    , (attrName "help" <> attrName "key"                                                   , active8)
    , (attrName "list" <> attrName "amount" <> attrName "decrease"                         , fg red8)
    , (attrName "list" <> attrName "amount" <> attrName "decrease" <> attrName "selected"  , selectNeg)
    , (attrName "list" <> attrName "balance"                                               , currentAttr & bold)
    , (attrName "list" <> attrName "balance" <> attrName "negative"                        , fg red8)
    , (attrName "list" <> attrName "balance" <> attrName "positive"                        , fg black8)
    , (attrName "list" <> attrName "balance" <> attrName "negative" <> attrName "selected" , selectNeg)
    , (attrName "list" <> attrName "balance" <> attrName "positive" <> attrName "selected" , select8)
    , (attrName "list" <> attrName "selected"                                              , select8)
  ])

  ,("greenterm", attrMap (green `on` black) [
    (attrName "list" <> attrName "selected"                             , black `on` green)
  ])

  ,("terminal", attrMap defAttr [
    (attrName "border"                                         , white `on` black),
    (attrName "list"                                           , defAttr),
    (attrName "list" <> attrName "selected"                             , defAttr & reverseVideo)
  ])

  ,("dark", attrMap (white `on` black & dim) [
      (attrName "border"                                                                   , white8 `on` black8)
    , (attrName "border" <> attrName "bold"                                                , currentAttr & bold)
    , (attrName "border" <> attrName "depth"                                               , active8)
    , (attrName "border" <> attrName "filename"                                            , currentAttr)
    , (attrName "border" <> attrName "key"                                                 , active8)
    , (attrName "border" <> attrName "minibuffer"                                          , white8 `on` black8 & bold)
    , (attrName "border" <> attrName "query"                                               , active8)
    , (attrName "border" <> attrName "selected"                                            , active8)
    , (attrName "error"                                                                    , fg red8)
    , (attrName "help"                                                                     , currentAttr & bold)
    , (attrName "help" <> attrName "heading"                                               , fg blue8)
    , (attrName "help" <> attrName "key"                                                   , active8)
    , (attrName "list" <> attrName "amount" <> attrName "decrease"                         , fg red8)
    , (attrName "list" <> attrName "amount" <> attrName "decrease" <> attrName "selected"  , red8 `on` black8 & bold)
    , (attrName "list" <> attrName "balance"                                               , currentAttr)
    , (attrName "list" <> attrName "balance" <> attrName "negative"                        , fg red8)
    , (attrName "list" <> attrName "balance" <> attrName "positive"                        , fg white8)
    , (attrName "list" <> attrName "balance" <> attrName "negative" <> attrName "selected" , red8 `on` black8    & bold)
    , (attrName "list" <> attrName "balance" <> attrName "positive" <> attrName "selected" , yellow8 `on` black8 & bold)
    , (attrName "list" <> attrName "selected"                                              , yellow8 `on` black8 & bold)
  ])

  ]
