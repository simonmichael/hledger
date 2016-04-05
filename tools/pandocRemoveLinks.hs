#!/usr/bin/env stack
{- stack runghc --verbosity info --package pandoc-types-1.16.1 -}

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter removeLinks

removeLinks :: Inline -> [Inline]
removeLinks (Link _ is _) = is
removeLinks x = [x]

