#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package pandoc
-}
-- Replace a table of contents marker
-- (a bullet list item containing "toc[-N[-M]]")
-- with a table of contents based on headings.
-- toc means full contents, toc-N means contents to depth N
-- and toc-N-M means contents from depth N to depth M.
-- Based on code from https://github.com/blaenk/blaenk.github.io

{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.List (groupBy)
import Data.List.Split
import Data.Tree (Forest, Tree(Node))
import Data.Monoid ((<>), mconcat)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Safe
import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc
import Text.Pandoc.JSON
import Text.Pandoc.Walk (walk, query)

main :: IO ()
main = toJSONFilter tableOfContents

tableOfContents :: Pandoc -> Pandoc
tableOfContents doc =
  let headers = query collectHeaders doc
  in walk (generateTOC headers) doc

collectHeaders :: Block -> [Block]
collectHeaders header@(Header _ (_, classes, _) _)
  | "notoc" `elem` classes = []
  | otherwise              = [header]
collectHeaders _ = []

generateTOC :: [Block] -> Block -> Block
generateTOC [] x = x
generateTOC headers x@(BulletList (( (( Plain ((Str txt):_)):_)):_)) =
  case tocParams txt of
  Just (mstartlevel, mendlevel) -> 
    render .
    forestDrop mstartlevel .
    forestPrune mendlevel .
    groupByHierarchy $
    headers -- (! A.class_ "right-toc") .
    where
      render = (RawBlock "html") . renderHtml . createTable
  Nothing -> x
generateTOC _ x = x

tocParams :: String -> Maybe (Maybe Int, Maybe Int)
tocParams s =
  case splitOn "-" s of
  ["toc"]                                    -> Just (Nothing, Nothing)
  ["toc",a]   | all isDigit a                -> Just (Nothing, readMay a)
  ["toc",a,b] | all isDigit a, all isDigit b -> Just (readMay a, readMay b)
  _                                          -> Nothing

forestDrop :: Maybe Int -> Forest a -> Forest a
forestDrop Nothing f = f
forestDrop (Just n) ts = concatMap (treeDrop n) ts

treeDrop :: Int -> Tree a -> Forest a
treeDrop n t | n < 1 = [t]
treeDrop n (Node _ ts) = concatMap (treeDrop (n-1)) ts

forestPrune :: Maybe Int -> Forest a -> Forest a
forestPrune Nothing f = f
forestPrune (Just n) ts = map (treePrune n) ts

treePrune :: Int -> Tree a -> Tree a
treePrune n t | n < 1 = t
treePrune n (Node v ts) = Node v $ map (treePrune (n-1)) ts

-- | remove all nodes past a certain depth
-- treeprune :: Int -> Tree a -> Tree a
-- treeprune 0 t = Node (root t) []
-- treeprune d t = Node (root t) (map (treeprune $ d-1) $ branches t)

groupByHierarchy :: [Block] -> Forest Block
groupByHierarchy = map (\(x:xs) -> Node x (groupByHierarchy xs)) . groupBy ((<) `on` headerLevel)

headerLevel :: Block -> Int
headerLevel (Header level _ _) = level
headerLevel _ = error "not a header"

createTable :: Forest Block -> H.Html
createTable headers =
  (H.nav ! A.id "toc") $ do
    H.p "Contents"
    H.ol $ markupHeaders headers

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

-- ignoreTOC :: Block -> Block
-- ignoreTOC (Header level (ident, classes, params) inline) =
--   Header level (ident, "notoc" : classes, params) inline
-- ignoreTOC x = x

-- removeTOCMarker :: Block -> Block
-- removeTOCMarker (BulletList (( (( Plain ((Str "toc"):_)):_)):_)) = Null
-- removeTOCMarker x = x

