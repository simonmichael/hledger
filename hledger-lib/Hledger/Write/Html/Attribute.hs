{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}      -- for stylesheet_
{- |
Helpers and CSS styles for HTML output.
-}
module Hledger.Write.Html.Attribute (
    stylesheet_,
    styles_,
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

import qualified Lucid.Base as LucidBase
import qualified Lucid
import qualified Data.Text as Text
import Data.Text (Text)


-- | result can be 'Lucid.Attribute' or @Lucid.Html ()@
stylesheet_ :: LucidBase.TermRaw Text result => [(Text,Text)] -> result
stylesheet_ elstyles =
    Lucid.style_ $ Text.unlines $
        "" : [el<>" {"<>styles<>"}" | (el,styles) <- elstyles]

styles_ :: [Text] -> Lucid.Attribute
styles_ = Lucid.style_ . Text.intercalate "; "

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
