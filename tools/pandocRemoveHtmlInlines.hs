#!/usr/bin/env stack
-- stack runghc --package pandoc-types

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter removeHtmlInlines

removeHtmlInlines :: Inline -> Inline
removeHtmlInlines (RawInline (Format "html") _) = Str ""
removeHtmlInlines x = x

