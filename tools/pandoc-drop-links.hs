#!/usr/bin/env stack
{- stack runghc --verbosity info --package pandoc-types -}

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter dropLinks

dropLinks :: Inline -> [Inline]
dropLinks (Link _ is _) = is
dropLinks x = [x]

