{-# LANGUAGE OverloadedStrings #-}
-- from https://github.com/blaenk/blaenk.github.io

module TableOfContents (
  tableOfContents,
  ignoreTOC,
  collectHeaders,
  removeTOCMarker
) where

import Text.Pandoc
import Text.Pandoc.Walk (walk, query)

import Data.List (groupBy)
import Data.Tree (Forest, Tree(Node))
import Data.Monoid ((<>), mconcat)
import Data.Function (on)
import Data.Maybe (fromMaybe)

import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

headerLevel :: Block -> Int
headerLevel (Header level _ _) = level
headerLevel _ = error "not a header"

ignoreTOC :: Block -> Block
ignoreTOC (Header level (ident, classes, params) inline) =
  Header level (ident, "notoc" : classes, params) inline
ignoreTOC x = x

removeTOCMarker :: Block -> Block
removeTOCMarker (BulletList (( (( Plain ((Str "toc"):_)):_)):_)) = Null
removeTOCMarker x = x

collectHeaders :: Block -> [Block]
collectHeaders header@(Header _ (_, classes, _) _) =
  if "notoc" `elem` classes
    then []
    else [header]
collectHeaders _ = []

groupByHierarchy :: [Block] -> Forest Block
groupByHierarchy = map (\(x:xs) -> Node x (groupByHierarchy xs)) . groupBy ((<) `on` headerLevel)

markupHeader :: Tree Block -> H.Html
markupHeader (Node (Header _ (ident, _, keyvals) inline) headers)
  | headers == [] = H.li $ link
  | otherwise     = H.li $ link <> (H.ol $ markupHeaders headers)
  where render x  = writeHtmlString def (Pandoc nullMeta [(Plain x)])
        section   = fromMaybe (render inline) (lookup "toc" keyvals)
        link      = H.a ! A.href (H.toValue $ "#" ++ ident) $ preEscapedToHtml section
markupHeader _ = error "what"

markupHeaders :: Forest Block -> H.Html
markupHeaders = mconcat . map markupHeader

createTable :: Forest Block -> H.Html
createTable headers =
  (H.nav ! A.id "toc") $ do
    H.p "Contents"
    H.ol $ markupHeaders headers

generateTOC :: [Block] -> String -> Block -> Block
generateTOC [] _     x = x
generateTOC headers alignment x@(BulletList (( (( Plain ((Str "toc"):_)):_)):_))
  | alignment == "right" = render . (! A.class_ "right-toc") . table $ headers
  | alignment == "left"  = render . table $ headers
  | otherwise            = x
  where render = (RawBlock "html") . renderHtml
        table  = createTable . groupByHierarchy
generateTOC _ _ x = x

tableOfContents :: String -> Pandoc -> Pandoc
tableOfContents alignment ast =
  if alignment /= "off"
    then let headers = query collectHeaders ast
         in walk (generateTOC headers alignment) ast
    else walk ignoreTOC ast

