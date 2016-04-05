#!/usr/bin/env stack
{- stack runghc --verbosity info --resolver lts-5.11 --package pandoc-types-1.16.1 -}

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter removeHtmlInlines

removeHtmlInlines :: Inline -> Inline
removeHtmlInlines (RawInline (Format "html") _) = Str ""
removeHtmlInlines x = x

