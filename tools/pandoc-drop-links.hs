#!/usr/bin/env stack
{- stack runghc --verbosity info --package pandoc-types-1.16.1 -}

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter dropLinks

dropLinks :: Inline -> [Inline]
dropLinks (Link _ is _) = is
dropLinks x = [x]

