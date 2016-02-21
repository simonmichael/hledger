#!/usr/bin/env stack
-- stack runghc --package pandoc-types

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter removeHtmlBlocks

removeHtmlBlocks :: Block -> Block
removeHtmlBlocks (RawBlock (Format "html") _) = Plain []
removeHtmlBlocks x = x
