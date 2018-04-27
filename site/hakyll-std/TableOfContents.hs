-- from https://github.com/blaenk/blaenk.github.io

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module TableOfContents (
  tableOfContents,
  ignoreTOC,
  collectHeaders,
  TOCAlignment(TOCOff,TOCLeft,TOCRight)
) where

import Text.Pandoc
import Text.Pandoc.Walk (walk, query)
import Text.Pandoc.Class (runPure)

import Data.List (groupBy)
import Data.Text (unpack)
import Data.Tree (Forest, Tree(Node))
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>), mconcat)
#endif
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)

import Text.Blaze.Html (preEscapedToHtml, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data TOCAlignment = TOCOff | TOCLeft | TOCRight

headerLevel :: Block -> Int
headerLevel (Header level _ _) = level
headerLevel _ = error "not a header"

ignoreTOC :: Block -> Block
ignoreTOC (Header level (ident, classes, params) inline) =
  Header level (ident, "notoc" : classes, params) inline
ignoreTOC x = x

collectHeaders :: Block -> [Block]
collectHeaders header@(Header _ (_, classes, _) _) =
  if "notoc" `elem` classes
    then []
    else [header]
collectHeaders _ = []

groupByHierarchy :: [Block] -> Forest Block
groupByHierarchy = map (\(x:xs) -> Node x (groupByHierarchy xs)) . groupBy ((<) `on` headerLevel)

markupHeader :: Tree Block -> H.Html
markupHeader n@(Node (Header _ (ident, _, keyvals) inline) headers)
  | headers == [] = H.li $ link
  | otherwise     = H.li $ link <> (H.ol $ markupHeaders headers)
  where render x  = case runPure $ writeHtml5String def (Pandoc nullMeta [(Plain x)]) of
                        Left  _   -> error $    "Error building header.\n"
                                             ++ "   saw: " ++ show n
                        Right txt -> txt
        section   = fromMaybe (unpack $ render inline) (lookup "toc" keyvals)
        link      = H.a ! A.href (H.toValue $ "#" ++ ident) $ preEscapedToHtml section
markupHeader n = error $    "'markupHeader' should only be passed a 'Node $ Header'\n"
                         ++ "    saw: " ++ show n

markupHeaders :: Forest Block -> H.Html
markupHeaders = mconcat . map markupHeader

createTable :: TOCAlignment -> Forest Block -> Block
createTable _ [] = Null
createTable alignment headers
    = render $ (H.nav ! (A.id "toc" <> alignmentAttr)) $ do
            H.p "Contents"
            H.ol $ markupHeaders headers
  where render = (RawBlock "html") . renderHtml
        alignmentAttr = case alignment of
                           TOCRight -> A.class_ "right-toc"
                           _        -> mempty

generateTOC :: [Block] -> TOCAlignment -> Block -> Block
generateTOC headers alignment x@(BulletList (( (( Plain ((Str "toc"):_)):_)):_))
    = createTable alignment . groupByHierarchy $ headers
generateTOC _ _ x = x

tableOfContents :: TOCAlignment -> Pandoc -> Pandoc
tableOfContents TOCOff    ast = walk ignoreTOC ast
tableOfContents alignment ast = let headers = query collectHeaders ast
                                 in walk (generateTOC headers alignment) ast
