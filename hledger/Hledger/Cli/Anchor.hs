{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
module Hledger.Cli.Anchor (
    setAccountAnchor,
    dateCell,
    dateSpanCell,
    headerDateSpanCell,
    ) where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time (Day)
import Data.Maybe (fromMaybe)

import qualified Text.URI as Uri
import qualified Text.URI.QQ as UriQQ

import qualified Hledger.Write.Spreadsheet as Spr
import Hledger.Write.Spreadsheet (headerCell)
import Hledger.Utils.Text (quoteIfSpaced)
import Hledger.Data.Dates (showDateSpan, showDate)
import Hledger.Data.Types (DateSpan)


registerQueryUrl :: [Text] -> Text
registerQueryUrl query =
    Uri.render $
    [UriQQ.uri|register|] {
        Uri.uriQuery =
            [Uri.QueryParam [UriQQ.queryKey|q|] $
             fromMaybe (error "register URI query construction failed") $
             Uri.mkQueryValue $ Text.unwords $
             map quoteIfSpaced $ filter (not . Text.null) query]
    }

{- |
>>> composeAnchor Nothing ["date:2024"]
""
>>> composeAnchor (Just "") ["date:2024"]
"register?q=date:2024"
>>> composeAnchor (Just "/") ["date:2024"]
"/register?q=date:2024"
>>> composeAnchor (Just "foo") ["date:2024"]
"foo/register?q=date:2024"
>>> composeAnchor (Just "foo/") ["date:2024"]
"foo/register?q=date:2024"
-}
composeAnchor :: Maybe Text -> [Text] -> Text
composeAnchor Nothing _ = mempty
composeAnchor (Just baseUrl) query =
    baseUrl <>
    (if all (('/'==) . snd) $ Text.unsnoc baseUrl then "" else "/") <>
    registerQueryUrl query

-- cf. Web.Widget.Common
removeDates :: [Text] -> [Text]
removeDates =
    filter (\term_ ->
        not $ Text.isPrefixOf "date:" term_ || Text.isPrefixOf "date2:" term_)

replaceDate :: Text -> [Text] -> [Text]
replaceDate prd query = "date:"<>prd : removeDates query

headerDateSpanCell ::
    Maybe Text -> [Text] -> DateSpan -> Spr.Cell () Text
headerDateSpanCell base query spn =
    let prd = showDateSpan spn in
    (headerCell prd) {
        Spr.cellAnchor = composeAnchor base $ replaceDate prd query
    }


dateQueryCell ::
    (Spr.Lines border) =>
    Maybe Text -> [Text] -> Text -> Text -> Spr.Cell border Text
dateQueryCell base query acct dateTerm =
    (Spr.defaultCell dateTerm) {
        Spr.cellAnchor =
            composeAnchor base $ "inacct:"<>acct : replaceDate dateTerm query
    }

dateCell ::
    (Spr.Lines border) =>
    Maybe Text -> [Text] -> Text -> Day -> Spr.Cell border Text
dateCell base query acct = dateQueryCell base query acct . showDate

dateSpanCell ::
    (Spr.Lines border) =>
    Maybe Text -> [Text] -> Text -> DateSpan -> Spr.Cell border Text
dateSpanCell base query acct = dateQueryCell base query acct . showDateSpan

setAccountAnchor ::
    Maybe Text -> [Text] -> Text -> Spr.Cell border text -> Spr.Cell border text
setAccountAnchor base query acct cell =
    cell {Spr.cellAnchor = composeAnchor base $ "inacct:"<>acct : query}
