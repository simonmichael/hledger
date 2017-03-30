#!/usr/bin/env stack
{- stack runghc --verbosity info --package pandoc-types -}

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter dropHtmlInlines

dropHtmlInlines :: Inline -> Inline
dropHtmlInlines (RawInline (Format "html") _) = Str ""
dropHtmlInlines x = x

