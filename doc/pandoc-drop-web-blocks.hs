#!/usr/bin/env stack
{- stack runghc --verbosity info --package pandoc-types-1.16.1 -}

import Text.Pandoc.Builder
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter dropWebBlocks

dropWebBlocks :: Block -> Block
dropWebBlocks (Div ("",["web"],[]) _) = Plain []
dropWebBlocks x = x
