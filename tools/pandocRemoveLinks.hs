#!/usr/bin/env stack
-- stack runghc --package pandoc-types

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter removeLinks

removeLinks :: Inline -> [Inline]
removeLinks (Link l _) = l
removeLinks x = [x]

