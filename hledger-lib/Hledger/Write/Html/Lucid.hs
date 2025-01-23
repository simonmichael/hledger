{-# LANGUAGE OverloadedStrings #-}
{- |
Export spreadsheet table data as HTML table.

This is derived from <https://hackage.haskell.org/package/classify-frog-0.2.4.3/src/src/Spreadsheet/Format.hs>
-}
module Hledger.Write.Html.Lucid (
    styledTableHtml,
    formatRow,
    formatCell,
    ) where

import qualified Hledger.Write.Html.Attribute as Attr
import qualified Hledger.Write.Spreadsheet as Spr
import Hledger.Write.Html (Lines, borderStyles)
import Hledger.Write.Spreadsheet (Type(..), Style(..), Emphasis(..), Cell(..))

import qualified Data.Text as Text
import qualified Lucid.Base as HtmlBase
import qualified Lucid as Html
import Data.Foldable (traverse_)


type Html = Html.Html ()

styledTableHtml :: (Lines border) => [[Cell border Html]] -> Html
styledTableHtml table = do
    Html.link_ [Html.rel_ "stylesheet", Html.href_ "hledger.css"]
    Html.style_ Attr.tableStylesheet
    Html.table_ $ traverse_ formatRow table

formatRow:: (Lines border) => [Cell border Html] -> Html
formatRow = Html.tr_ . traverse_ formatCell

formatCell :: (Lines border) => Cell border Html -> Html
formatCell cell =
    let str = cellContent cell in
    let content =
            if Text.null $ cellAnchor cell
                then str
                else Html.a_ [Html.href_ $ cellAnchor cell] str in
    let style =
            case borderStyles cell of
                [] -> []
                ss -> [Html.style_ $ Attr.concatStyles ss] in
    let class_ =
            map Html.class_ $
            filter (not . Text.null) [Spr.textFromClass $ cellClass cell] in
    let span_ makeCell attrs cont =
            case Spr.cellSpan cell of
                Spr.NoSpan -> makeCell attrs cont
                Spr.Covered -> pure ()
                Spr.SpanHorizontal n ->
                    makeCell (Html.colspan_ (Text.pack $ show n) : attrs) cont
                Spr.SpanVertical n ->
                    makeCell (Html.rowspan_ (Text.pack $ show n) : attrs) cont
            in
    case cellStyle cell of
        Head -> span_ Html.th_ (style++class_) content
        Body emph ->
            let align =
                    case cellType cell of
                        TypeString -> []
                        TypeDate -> []
                        _ -> [HtmlBase.makeAttribute "align" "right"]
                valign =
                    case Spr.cellSpan cell of
                        Spr.SpanVertical n ->
                            if n>1
                                then [HtmlBase.makeAttribute "valign" "top"]
                                else []
                        _ -> []
                withEmph =
                    case emph of
                        Item -> id
                        Total -> Html.b_
            in  span_ Html.td_ (style++align++valign++class_) $
                withEmph content
