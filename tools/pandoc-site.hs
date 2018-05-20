{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import Text.Pandoc
import Text.Pandoc.Walk (query)
import Text.Pandoc.Builder (text, toList)
import Text.Pandoc.JSON (toJSONFilter)
import Text.Pandoc.Shared (hierarchicalize, Element(..))

import Data.Maybe (fromMaybe)

collectHeaders :: Block -> [Block]
collectHeaders header@(Header _ (_, classes, _) _) =
  if "notoc" `elem` classes
    then []
    else [header]
collectHeaders _ = []

markupLink :: Attr -> [Inline] -> Inline
markupLink (headerId, _, headerProperties) headerText
    = let linkText = fromMaybe headerText (fmap (toList . text) $ lookup "toc" headerProperties)
       in Link nullAttr linkText (("#" ++ headerId), headerId)

markupElement :: Element -> [Block]
markupElement (Sec _ _ hAttr hText headers)
  | headers == [] = [link]
  | otherwise     = [link, markupElements headers]
  where link = Plain [markupLink hAttr hText]
markupElement n = error $    "'markupElement' should only be passed a 'Sec'\n"
                          ++ "    saw: " ++ show n

markupElements :: [Element] -> Block
markupElements = OrderedList (1, Decimal, Period) . map markupElement

createTable :: [Element] -> [Block]
createTable [] = []
createTable headers
    = let navBegin  = RawBlock "html" "<nav id=\"toc\" class=\"right-toc\">"
          navEnd    = RawBlock "html" "</nav>"
       in [navBegin, Para [Str "Contents"], markupElements headers, navEnd]

generateTOC :: [Block] -> Block -> [Block]
generateTOC toc (Para [Str "$toc$"]) = toc
generateTOC _   x                    = [x]

tableOfContents :: Pandoc -> Pandoc
tableOfContents (Pandoc meta blks)
    = let headers = query collectHeaders blks
          toc     = createTable . hierarchicalize $ headers
       in Pandoc meta (concatMap (generateTOC toc) blks)

main :: IO ()
main = toJSONFilter tableOfContents
