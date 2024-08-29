{-# LANGUAGE OverloadedStrings #-}
{- |
Export spreadsheet table data as HTML table.

This is derived from <https://hackage.haskell.org/package/classify-frog-0.2.4.3/src/src/Spreadsheet/Format.hs>
-}
module Hledger.Write.Html (
    printHtml,
    ) where

import qualified Hledger.Write.Spreadsheet as Spr
import Hledger.Write.Spreadsheet (Type(..), Style(..), Emphasis(..), Cell(..))

import qualified Data.Text as Text
import qualified Lucid.Base as LucidBase
import qualified Lucid
import Data.Text (Text)
import Data.Foldable (for_)


printHtml :: (Lines border) => [[Cell border (Lucid.Html ())]] -> Lucid.Html ()
printHtml table = do
    Lucid.style_ $ Text.unlines $
        "" :
        "table {border-collapse:collapse}" :
        "th, td {padding-left:1em}" :
        "th.account, td.account {padding-left:0;}" :
        []
    Lucid.table_ $ for_ table $ \row ->
        Lucid.tr_ $ for_ row $ \cell ->
        formatCell cell

formatCell :: (Lines border) => Cell border (Lucid.Html ()) -> Lucid.Html ()
formatCell cell =
    let str = cellContent cell in
    let border field access =
            map (field<>) $ borderLines $ access $ cellBorder cell in
    let leftBorder   = border "border-left:"   Spr.borderLeft   in
    let rightBorder  = border "border-right:"  Spr.borderRight  in
    let topBorder    = border "border-top:"    Spr.borderTop    in
    let bottomBorder = border "border-bottom:" Spr.borderBottom in
    let style =
            case leftBorder++rightBorder++topBorder++bottomBorder of
                [] -> []
                ss -> [Lucid.style_ $ Text.intercalate "; " ss] in
    case cellStyle cell of
        Head -> Lucid.th_ style str
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
            in  Lucid.td_ (style++align) $ withEmph str


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
