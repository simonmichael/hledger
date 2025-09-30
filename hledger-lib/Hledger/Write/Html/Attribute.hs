{-# LANGUAGE OverloadedStrings #-}
{- |
Helpers and CSS styles for HTML output.
-}
module Hledger.Write.Html.Attribute (
    stylesheet,
    concatStyles,
    tableStylesheet,
    tableStyle,
    bold,
    doubleborder,
    topdoubleborder,
    bottomdoubleborder,
    alignright,
    alignleft,
    aligncenter,
    collapse,
    lpad,
    rpad,
    hpad,
    vpad,
    ) where

import Data.Text qualified as Text
import Data.Text (Text)


stylesheet :: [(Text,Text)] -> Text
stylesheet elstyles =
    Text.unlines $
        "" : [el<>" {"<>styles<>"}" | (el,styles) <- elstyles]

concatStyles :: [Text] -> Text
concatStyles = Text.intercalate "; "


tableStylesheet :: Text
tableStylesheet = stylesheet tableStyle

tableStyle :: [(Text, Text)]
tableStyle =
  [("table", collapse),
   ("th, td", lpad),
   ("th.account, td.account", "padding-left:0;")]

bold, doubleborder, topdoubleborder, bottomdoubleborder :: Text
bold = "font-weight:bold"
doubleborder = "double black"
topdoubleborder    = "border-top:"<>doubleborder
bottomdoubleborder = "border-bottom:"<>doubleborder

alignright, alignleft, aligncenter :: Text
alignright  = "text-align:right"
alignleft   = "text-align:left"
aligncenter = "text-align:center"

collapse :: Text
collapse = "border-collapse:collapse"

lpad, rpad, hpad, vpad :: Text
lpad = "padding-left:1em"
rpad = "padding-right:1em"
hpad = "padding-left:1em; padding-right:1em"
vpad = "padding-top:1em;  padding-bottom:1em"
