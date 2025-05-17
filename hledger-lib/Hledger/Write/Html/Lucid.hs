{-# LANGUAGE OverloadedStrings #-}
{- |
HTML writing helpers using lucid.
-}

module Hledger.Write.Html.Lucid (
    Html,
    L.toHtml,
    styledTableHtml,
    formatRow,
    formatCell,
    ) where

import           Data.Foldable (traverse_)
import qualified Data.Text as Text
import qualified Lucid.Base as L
import qualified Lucid as L

import qualified Hledger.Write.Html.Attribute as Attr
import           Hledger.Write.Html.HtmlCommon
import           Hledger.Write.Spreadsheet (Type(..), Style(..), Emphasis(..), Cell(..))
import qualified Hledger.Write.Spreadsheet as Spr


type Html = L.Html ()

-- | Export spreadsheet table data as HTML table.
-- This is derived from <https://hackage.haskell.org/package/classify-frog-0.2.4.3/src/src/Spreadsheet/Format.hs>
styledTableHtml :: (Lines border) => [[Cell border Html]] -> Html
styledTableHtml table = do
    L.link_ [L.rel_ "stylesheet", L.href_ "hledger.css"]
    L.style_ Attr.tableStylesheet
    L.table_ $ traverse_ formatRow table

formatRow:: (Lines border) => [Cell border Html] -> Html
formatRow = L.tr_ . traverse_ formatCell

formatCell :: (Lines border) => Cell border Html -> Html
formatCell cell =
    let str = cellContent cell in
    let content =
            if Text.null $ cellAnchor cell
                then str
                else L.a_ [L.href_ $ cellAnchor cell] str in
    let style =
            case borderStyles cell of
                [] -> []
                ss -> [L.style_ $ Attr.concatStyles ss] in
    let class_ =
            map L.class_ $
            filter (not . Text.null) [Spr.textFromClass $ cellClass cell] in
    let span_ makeCell attrs cont =
            case Spr.cellSpan cell of
                Spr.NoSpan -> makeCell attrs cont
                Spr.Covered -> pure ()
                Spr.SpanHorizontal n ->
                    makeCell (L.colspan_ (Text.pack $ show n) : attrs) cont
                Spr.SpanVertical n ->
                    makeCell (L.rowspan_ (Text.pack $ show n) : attrs) cont
            in
    case cellStyle cell of
        Head -> span_ L.th_ (style++class_) content
        Body emph ->
            let align =
                    case cellType cell of
                        TypeString -> []
                        TypeDate -> []
                        _ -> [L.makeAttribute "align" "right"]
                valign =
                    case Spr.cellSpan cell of
                        Spr.SpanVertical n ->
                            if n>1
                                then [L.makeAttribute "valign" "top"]
                                else []
                        _ -> []
                withEmph =
                    case emph of
                        Item -> id
                        Total -> L.b_
            in  span_ L.td_ (style++align++valign++class_) $
                withEmph content

