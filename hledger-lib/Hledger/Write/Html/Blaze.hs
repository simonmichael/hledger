{-# LANGUAGE OverloadedStrings #-}
{- |
Export spreadsheet table data as HTML table.

This is derived from <https://hackage.haskell.org/package/classify-frog-0.2.4.3/src/src/Spreadsheet/Format.hs>
-}
module Hledger.Write.Html.Blaze (
    printHtml,
    formatRow,
    formatCell,
    ) where

import qualified Hledger.Write.Html.Attribute as Attr
import qualified Hledger.Write.Spreadsheet as Spr
import Hledger.Write.Spreadsheet (Type(..), Style(..), Emphasis(..), Cell(..))

import qualified Text.Blaze.Html4.Transitional.Attributes as HtmlAttr
import qualified Text.Blaze.Html4.Transitional as Html
import qualified Data.Text as Text
import Text.Blaze.Html4.Transitional (Html, toHtml, (!))
import Data.Text (Text)
import Data.Foldable (traverse_)


printHtml :: (Lines border) => [[Cell border Html]] -> Html
printHtml table = do
    Html.style $ toHtml $ Attr.tableStylesheet
    Html.table $ traverse_ formatRow table

formatRow:: (Lines border) => [Cell border Html] -> Html
formatRow = Html.tr . traverse_ formatCell

formatCell :: (Lines border) => Cell border Html -> Html
formatCell cell =
    let str = cellContent cell in
    let content =
            if Text.null $ cellAnchor cell
                then str
                else Html.a str !
                        HtmlAttr.href (Html.textValue (cellAnchor cell)) in
    let border field access =
            map (field<>) $ borderLines $ access $ cellBorder cell in
    let leftBorder   = border "border-left:"   Spr.borderLeft   in
    let rightBorder  = border "border-right:"  Spr.borderRight  in
    let topBorder    = border "border-top:"    Spr.borderTop    in
    let bottomBorder = border "border-bottom:" Spr.borderBottom in
    let style =
            case leftBorder++rightBorder++topBorder++bottomBorder of
                [] -> []
                ss -> [HtmlAttr.style $ Html.textValue $
                        Attr.concatStyles ss] in
    let class_ =
            map (HtmlAttr.class_ . Html.textValue) $
            filter (not . Text.null) [Spr.textFromClass $ cellClass cell] in
    case cellStyle cell of
        Head -> foldl (!) (Html.th content) (style++class_)
        Body emph ->
            let align =
                    case cellType cell of
                        TypeString -> []
                        TypeDate -> []
                        _ -> [HtmlAttr.align "right"]
                valign = [HtmlAttr.valign "top"]
                withEmph =
                    case emph of
                        Item -> id
                        Total -> Html.b
            in  foldl (!) (Html.td $ withEmph content)
                    (style++align++valign++class_)


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
