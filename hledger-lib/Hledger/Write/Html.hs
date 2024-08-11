{-# LANGUAGE OverloadedStrings #-}
{- |
Export spreadsheet table data as HTML table.

This is derived from <https://hackage.haskell.org/package/classify-frog-0.2.4.3/src/src/Spreadsheet/Format.hs>
-}
module Hledger.Write.Html (
    printHtml,
    ) where

import Hledger.Write.Spreadsheet (Type(..), Style(..), Emphasis(..), Cell(..))

import qualified Lucid.Base as LucidBase
import qualified Lucid
import Data.Foldable (for_)


printHtml :: [[Cell (Lucid.Html ())]] -> Lucid.Html ()
printHtml table =
    Lucid.table_ $ for_ table $ \row ->
    Lucid.tr_ $ for_ row $ \cell ->
    formatCell cell

formatCell :: Cell (Lucid.Html ()) -> Lucid.Html ()
formatCell cell =
    let str = cellContent cell in
    case cellStyle cell of
        Head -> Lucid.th_ str
        Body emph ->
            let align =
                    case cellType cell of
                        TypeString -> []
                        TypeDate -> []
                        _ -> [LucidBase.makeAttribute "align" "right"]
                withEmph =
                    case emph of
                        Item -> id
                        Total -> Lucid.b_
            in  Lucid.td_ align $ withEmph str
