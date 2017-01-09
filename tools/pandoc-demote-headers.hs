#!/usr/bin/env stack
{- stack runghc --verbosity info --package pandoc-types-1.16.1 -}

import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Data.Char (toUpper)

main :: IO ()
main = toJSONFilter demoteHeaders

demoteHeaders :: Block -> Block
demoteHeaders (Header l attr xs) = Header (l+1) attr xs
demoteHeaders x = x

