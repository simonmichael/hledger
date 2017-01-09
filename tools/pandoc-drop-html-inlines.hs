#!/usr/bin/env stack
{- stack runghc --verbosity info --package pandoc-types-1.16.1 -}

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter dropHtmlInlines

dropHtmlInlines :: Inline -> Inline
dropHtmlInlines (RawInline (Format "html") _) = Str ""
dropHtmlInlines x = x

