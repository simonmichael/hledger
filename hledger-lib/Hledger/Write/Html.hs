{- |
Export spreadsheet table data as HTML table.

This is derived from <https://hackage.haskell.org/package/classify-frog-0.2.4.3/src/src/Spreadsheet/Format.hs>
-}
module Hledger.Write.Html (
    printHtml,
    ) where

import Hledger.Write.Spreadsheet (Type(..), Style(..), Emphasis(..), Cell(..))

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Text.Printf (printf)


printHtml :: [[Cell]] -> TL.Text
printHtml table =
    TL.unlines $ map (TL.fromStrict . T.pack) $
    "<table>" :
    (table >>= \row ->
        "<tr>" :
        (row >>= formatCell) ++
        "</tr>" :
        []) ++
    "</table>" :
    []

formatCell :: Cell -> [String]
formatCell cell =
    (let str = escape $ T.unpack $ cellContent cell in
     case cellStyle cell of
        Head -> printf "<th>%s</th>" str
        Body emph ->
            let align =
                    case cellType cell of
                        TypeString -> ""
                        _ -> " align=right"
                (emphOpen, emphClose) =
                    case emph of
                        Item -> ("", "")
                        Total -> ("<b>", "</b>")
            in  printf "<td%s>%s%s%s</td>" align emphOpen str emphClose) :
    []


escape :: String -> String
escape =
    concatMap $ \c ->
        case c of
            '\n' -> "<br>"
            '&' -> "&amp;"
            '<' -> "&lt;"
            '>' -> "&gt;"
            '"' -> "&quot;"
            '\'' -> "&apos;"
            _ -> [c]
