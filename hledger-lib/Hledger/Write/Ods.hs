{- |
Export table data as OpenDocument Spreadsheet
<https://docs.oasis-open.org/office/OpenDocument/v1.3/>.
This format supports character encodings, fixed header rows and columns,
number formatting, text styles, merged cells, formulas, hyperlinks.
Currently we support Flat ODS, a plain uncompressed XML format.

This is derived from <https://hackage.haskell.org/package/classify-frog-0.2.4.3/src/src/Spreadsheet/Format.hs>
-}
module Hledger.Write.Ods where

import Hledger.Data.Types (CommoditySymbol, Amount, AmountPrecision(..))
import Hledger.Data.Types (acommodity, aquantity, astyle, asprecision)

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (mapMaybe)

import qualified System.IO as IO
import Text.Printf (printf)


data Type =
      TypeString
    | TypeAmount !Amount
    | TypeMixedAmount
    deriving (Eq, Ord, Show)

data Style = Ordinary | Head | Foot
    deriving (Eq, Ord, Show)

data Cell =
    Cell {
        cellType :: Type,
        cellStyle :: Style,
        cellContent :: Text
    }

defaultCell :: Cell
defaultCell =
    Cell {
        cellType = TypeString,
        cellStyle = Ordinary,
        cellContent = T.empty
    }


printFods ::
    IO.TextEncoding -> Map Text ((Maybe Int, Maybe Int), [[Cell]]) -> TL.Text
printFods encoding tables =
    let fileOpen customStyles =
          map (map (\c -> case c of '\'' -> '"'; _ -> c)) $
          printf "<?xml version='1.0' encoding='%s'?>" (show encoding) :
          "<office:document" :
          "  office:mimetype='application/vnd.oasis.opendocument.spreadsheet'" :
          "  office:version='1.3'" :
          "  xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'" :
          "  xmlns:xsd='http://www.w3.org/2001/XMLSchema'" :
          "  xmlns:text='urn:oasis:names:tc:opendocument:xmlns:text:1.0'" :
          "  xmlns:style='urn:oasis:names:tc:opendocument:xmlns:style:1.0'" :
          "  xmlns:meta='urn:oasis:names:tc:opendocument:xmlns:meta:1.0'" :
          "  xmlns:config='urn:oasis:names:tc:opendocument:xmlns:config:1.0'" :
          "  xmlns:xlink='http://www.w3.org/1999/xlink'" :
          "  xmlns:fo='urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0'" :
          "  xmlns:ooo='http://openoffice.org/2004/office'" :
          "  xmlns:office='urn:oasis:names:tc:opendocument:xmlns:office:1.0'" :
          "  xmlns:table='urn:oasis:names:tc:opendocument:xmlns:table:1.0'" :
          "  xmlns:number='urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0'" :
          "  xmlns:of='urn:oasis:names:tc:opendocument:xmlns:of:1.2'" :
          "  xmlns:field='urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0'" :
          "  xmlns:form='urn:oasis:names:tc:opendocument:xmlns:form:1.0'>" :
          "<office:styles>" :
          "  <style:style style:name='head' style:family='table-cell'>" :
          "    <style:paragraph-properties fo:text-align='center'/>" :
          "    <style:text-properties fo:font-weight='bold'/>" :
          "  </style:style>" :
          "  <style:style style:name='foot' style:family='table-cell'>" :
          "    <style:text-properties fo:font-weight='bold'/>" :
          "  </style:style>" :
          "  <style:style style:name='amount' style:family='table-cell'>" :
          "    <style:paragraph-properties fo:text-align='end'/>" :
          "  </style:style>" :
          "  <style:style style:name='total-amount' style:family='table-cell'>" :
          "    <style:paragraph-properties fo:text-align='end'/>" :
          "    <style:text-properties fo:font-weight='bold'/>" :
          "  </style:style>" :
          customStyles ++
          "</office:styles>" :
          []

        fileClose =
          "</office:document>" :
          []

        tableConfig tableNames =
          " <office:settings>" :
          "  <config:config-item-set config:name='ooo:view-settings'>" :
          "   <config:config-item-map-indexed config:name='Views'>" :
          "    <config:config-item-map-entry>" :
          "     <config:config-item-map-named config:name='Tables'>" :
          (fold $
           flip Map.mapWithKey tableNames $ \tableName (mTopRow,mLeftColumn) ->
             printf "      <config:config-item-map-entry config:name='%s'>" tableName :
             (flip foldMap mLeftColumn $ \leftColumn ->
                "       <config:config-item config:name='HorizontalSplitMode' config:type='short'>2</config:config-item>" :
                printf "       <config:config-item config:name='HorizontalSplitPosition' config:type='int'>%d</config:config-item>" leftColumn :
                printf "       <config:config-item config:name='PositionRight' config:type='int'>%d</config:config-item>" leftColumn :
                []) ++
             (flip foldMap mTopRow $ \topRow ->
                "       <config:config-item config:name='VerticalSplitMode' config:type='short'>2</config:config-item>" :
                printf "       <config:config-item config:name='VerticalSplitPosition' config:type='int'>%d</config:config-item>" topRow :
                printf "       <config:config-item config:name='PositionBottom' config:type='int'>%d</config:config-item>" topRow :
                []) ++
             "      </config:config-item-map-entry>" :
             []) ++
          "     </config:config-item-map-named>" :
          "    </config:config-item-map-entry>" :
          "   </config:config-item-map-indexed>" :
          "  </config:config-item-set>" :
          " </office:settings>" :
          []

        tableOpen name =
          "<office:body>" :
          "<office:spreadsheet>" :
          printf "<table:table table:name='%s'>" name :
          []

        tableClose =
          "</table:table>" :
          "</office:spreadsheet>" :
          "</office:body>" :
          []

    in  TL.unlines $ map (TL.fromStrict . T.pack) $
        fileOpen
          (numberConfig
              =<< Set.toList (numberStyles (foldMap (concat.snd) tables))) ++
        tableConfig (fmap fst tables) ++
        (Map.toAscList tables >>= \(name,(_,table)) ->
            tableOpen name ++
            (table >>= \row ->
                "<table:table-row>" :
                (row >>= formatCell) ++
                "</table:table-row>" :
                []) ++
            tableClose) ++
        fileClose


numberStyles :: [Cell] -> Set (CommoditySymbol, AmountPrecision)
numberStyles =
    Set.fromList .
    mapMaybe (\cell ->
        case cellType cell of
            TypeAmount amt -> Just (acommodity amt, asprecision $ astyle amt)
            _ -> Nothing)

numberStyleName :: (CommoditySymbol, AmountPrecision) -> String
numberStyleName (comm, prec) =
    printf "%s-%s" comm $
    case prec of
        NaturalPrecision -> "natural"
        Precision k -> show k

numberConfig :: (CommoditySymbol, AmountPrecision) -> [String]
numberConfig (comm, prec) =
    let precStr =
            case prec of
                NaturalPrecision -> ""
                Precision k -> printf " number:decimal-places='%d'" k
        name = numberStyleName (comm, prec)
    in
    printf "  <number:number-style style:name='number-%s'>" name :
    printf "    <number:number number:min-integer-digits='1'%s/>" precStr :
    printf "    <number:text>%s%s</number:text>"
      (if T.null comm then "" else " ") comm :
    "  </number:number-style>" :
    "  <style:style style:family='table-cell'" :
    printf "    style:name='%s' style:data-style-name='number-%s'/>" name name :
    []


formatCell :: Cell -> [String]
formatCell cell =
    let style, valueType :: String
        style =
          case (cellStyle cell, cellType cell) of
            (Ordinary, TypeString) -> ""
            (Ordinary, TypeMixedAmount) -> " table:style-name='amount'"
            (Ordinary, TypeAmount amt) -> numberStyle amt
            (Foot, TypeString) -> " table:style-name='foot'"
            (Foot, _) -> " table:style-name='total-amount'"
            (Head, _) -> " table:style-name='head'"

        numberStyle amt =
            printf " table:style-name='%s'"
                (numberStyleName (acommodity amt, asprecision $ astyle amt))
        valueType =
            case cellType cell of
                TypeAmount amt ->
                    printf
                        "office:value-type='float' office:value='%s'"
                        (show $ aquantity amt)
                _ -> "office:value-type='string'"

    in
    printf "<table:table-cell%s %s>" style valueType :
    printf "<text:p>%s</text:p>" (escape $ T.unpack $ cellContent cell) :
    "</table:table-cell>" :
    []

escape :: String -> String
escape =
    concatMap $ \c ->
        case c of
            '\n' -> "&#10;"
            '&' -> "&amp;"
            '<' -> "&lt;"
            '>' -> "&gt;"
            '"' -> "&quot;"
            '\'' -> "&apos;"
            _ -> [c]
