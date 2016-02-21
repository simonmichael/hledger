#!/usr/bin/env stack
-- stack runghc --package pandoc-types

import Text.Pandoc.Builder
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter removeManonlyBlocks

removeManonlyBlocks :: Block -> Block
removeManonlyBlocks (Div ("",["manonly"],[]) _) = Plain []
removeManonlyBlocks x = x
