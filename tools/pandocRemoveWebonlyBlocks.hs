#!/usr/bin/env stack
-- stack runghc --package pandoc-types

import Text.Pandoc.Builder
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter removeWebonlyBlocks

removeWebonlyBlocks :: Block -> Block
removeWebonlyBlocks (Div ("",["webonly"],[]) _) = Plain []
removeWebonlyBlocks x = x
