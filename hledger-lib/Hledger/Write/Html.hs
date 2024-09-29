{-# LANGUAGE OverloadedStrings #-}
{- |
Common definitions for Html.Blaze and Html.Lucid
-}
module Hledger.Write.Html (
    Lines(..),
    borderStyles,
    ) where

import qualified Hledger.Write.Spreadsheet as Spr
import Hledger.Write.Spreadsheet (Cell(..))

import Data.Text (Text)


borderStyles :: Lines border => Cell border text -> [Text]
borderStyles cell =
    let border field access =
            map (field<>) $ borderLines $ access $ cellBorder cell in
    let leftBorder   = border "border-left:"   Spr.borderLeft   in
    let rightBorder  = border "border-right:"  Spr.borderRight  in
    let topBorder    = border "border-top:"    Spr.borderTop    in
    let bottomBorder = border "border-bottom:" Spr.borderBottom in
    leftBorder++rightBorder++topBorder++bottomBorder


class (Spr.Lines border) => Lines border where
    borderLines :: border -> [Text]

instance Lines () where
    borderLines () = []

instance Lines Spr.NumLines where
    borderLines prop =
        case prop of
            Spr.NoLine -> []
            Spr.SingleLine -> ["black"]
            Spr.DoubleLine -> ["double black"]
