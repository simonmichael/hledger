#!/usr/bin/env stack
{- stack runghc --verbosity info --package pandoc-types-1.16.1 -}

import Text.Pandoc.Builder
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter dropManonlyBlocks

dropManonlyBlocks :: Block -> Block
dropManonlyBlocks (Div ("",["manonly"],[]) _) = Plain []
dropManonlyBlocks x = x
