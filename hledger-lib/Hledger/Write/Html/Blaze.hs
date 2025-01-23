{-# LANGUAGE OverloadedStrings #-}
{- |
Export spreadsheet table data as HTML table.

This is derived from <https://hackage.haskell.org/package/classify-frog-0.2.4.3/src/src/Spreadsheet/Format.hs>
-}
module Hledger.Write.Html.Blaze (
    styledTableHtml,
    formatRow,
    formatCell,
    ) where

import qualified Hledger.Write.Html.Attribute as Attr
import qualified Hledger.Write.Spreadsheet as Spr
import Hledger.Write.Html (Lines, borderStyles)
import Hledger.Write.Spreadsheet (Type(..), Style(..), Emphasis(..), Cell(..))

import qualified Text.Blaze.Html4.Transitional.Attributes as HtmlAttr
import qualified Text.Blaze.Html4.Transitional as Html
import qualified Data.Text as Text
import Text.Blaze.Html4.Transitional (Html, toHtml, (!))
import Data.Foldable (traverse_)


styledTableHtml :: (Lines border) => [[Cell border Html]] -> Html
styledTableHtml table = do
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
    let style =
            case borderStyles cell of
                [] -> []
                ss -> [HtmlAttr.style $ Html.textValue $
                        Attr.concatStyles ss] in
    let class_ =
            map (HtmlAttr.class_ . Html.textValue) $
            filter (not . Text.null) [Spr.textFromClass $ cellClass cell] in
    let span_ makeCell attrs =
            case Spr.cellSpan cell of
                Spr.NoSpan -> foldl (!) makeCell attrs
                Spr.Covered -> pure ()
                Spr.SpanHorizontal n ->
                    foldl (!) makeCell
                        (HtmlAttr.colspan (Html.stringValue $ show n) : attrs)
                Spr.SpanVertical n ->
                    foldl (!) makeCell
                        (HtmlAttr.rowspan (Html.stringValue $ show n) : attrs)
            in
    case cellStyle cell of
        Head -> span_ (Html.th content) (style++class_)
        Body emph ->
            let align =
                    case cellType cell of
                        TypeString -> []
                        TypeDate -> []
                        _ -> [HtmlAttr.align "right"]
                valign =
                    case Spr.cellSpan cell of
                        Spr.SpanVertical n ->
                            if n>1 then [HtmlAttr.valign "top"] else []
                        _ -> []
                withEmph =
                    case emph of
                        Item -> id
                        Total -> Html.b
            in  span_ (Html.td $ withEmph content) $
                style++align++valign++class_
