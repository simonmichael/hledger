#!/usr/bin/env stack
{- stack runghc --verbosity info --package pandoc-types-1.16.1 -}

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter dropHtmlBlocks

dropHtmlBlocks :: Block -> Block
dropHtmlBlocks (RawBlock (Format "html") _) = Plain []
dropHtmlBlocks x = x
