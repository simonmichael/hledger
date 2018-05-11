{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import Text.Pandoc
import Text.Pandoc.Walk (walk, query)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Builder (text, toList)
import Text.Pandoc.Options (def)
import Text.Pandoc.JSON (toJSONFilter)

import Data.Either (fromRight)
import Data.List (groupBy)
import Data.Tree (Forest, Tree(Node))
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)

data TOCAlignment = TOCOff | TOCLeft | TOCRight

headerLevel :: Block -> Int
headerLevel (Header level _ _) = level
headerLevel _ = error "not a header"

ignoreTOC :: Block -> Block
ignoreTOC (Header level (ident, classes, params) inlines) =
  Header level (ident, "notoc" : classes, params) inlines
ignoreTOC x = x

collectHeaders :: Block -> [Block]
collectHeaders header@(Header _ (_, classes, _) _) =
  if "notoc" `elem` classes
    then []
    else [header]
collectHeaders _ = []

groupByHierarchy :: [Block] -> Forest Block
groupByHierarchy = map (\(x:xs) -> Node x (groupByHierarchy xs)) . groupBy ((<) `on` headerLevel)

markupLink :: Attr -> [Inline] -> Inline
markupLink (headerId, _, headerProperties) headerText
    = let linkText = fromMaybe headerText (fmap (toList . text) $ lookup "toc" headerProperties)
       in Link nullAttr linkText (("#" ++ headerId), headerId)

markupHeader :: Tree Block -> [Block]
markupHeader n@(Node (Header _ hAttr hText) headers)
  | headers == [] = [link]
  | otherwise     = [link, markupHeaders headers]
  where link = Plain [markupLink hAttr hText]
markupHeader n = error $    "'markupHeader' should only be passed a 'Node $ Header'\n"
                         ++ "    saw: " ++ show n

markupHeaders :: Forest Block -> Block
markupHeaders = OrderedList (1, Decimal, Period) . map markupHeader

createTable :: TOCAlignment -> Forest Block -> Block
createTable _ [] = Null
createTable alignment headers
    = let alignAttr = case alignment of
                        TOCRight -> " class=\"right-toc\""
                        _        -> ""
          navBegin  = "<nav id=\"toc\"" ++ alignAttr ++ ">"
          navEnd    = "</nav>"
          tocDoc    = Pandoc nullMeta [Para [Str "Contents"], markupHeaders headers]
          tocString = unpack . fromRight mempty . runPure . writeHtml5String def $ tocDoc
       in RawBlock "html" (navBegin ++ "\n" ++ tocString ++ "\n" ++ navEnd)

generateTOC :: Block -> Block -> Block
generateTOC toc (BulletList (( (( Plain ((Str "toc"):_)):_)):_))
                = toc
generateTOC _ x = x

tableOfContents :: TOCAlignment -> Pandoc -> Pandoc
tableOfContents TOCOff ast = walk ignoreTOC ast
tableOfContents alignment ast
    = let headers = query collectHeaders ast
          toc     = createTable alignment . groupByHierarchy $ headers
       in walk (generateTOC toc) ast

main :: IO ()
main = toJSONFilter (tableOfContents TOCRight)
