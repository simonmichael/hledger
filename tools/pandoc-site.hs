{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import Text.Pandoc
import Text.Pandoc.Walk (walk, query)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Builder (text, toList)
import Text.Pandoc.Options (def)
import Text.Pandoc.JSON (toJSONFilter)
import Text.Pandoc.Shared (hierarchicalize, Element(..))

import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)

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

createTable :: [Element] -> Block
createTable [] = Null
createTable headers
    = let navBegin  = "<nav id=\"toc\" class=\"right-toc\">"
          navEnd    = "</nav>"
          tocDoc    = Pandoc nullMeta [Para [Str "Contents"], markupElements headers]
          tocString = unpack . fromRight mempty . runPure . writeHtml5String def $ tocDoc
       in RawBlock "html" (navBegin ++ "\n" ++ tocString ++ "\n" ++ navEnd)

generateTOC :: Block -> Block -> Block
generateTOC toc (Para [Str "$toc$"]) = toc
generateTOC _   x                    = x

tableOfContents :: Pandoc -> Pandoc
tableOfContents ast
    = let headers = query collectHeaders ast
          toc     = createTable . hierarchicalize $ headers
       in walk (generateTOC toc) ast

main :: IO ()
main = toJSONFilter tableOfContents
