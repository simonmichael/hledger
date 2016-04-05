#!/usr/bin/env stack
{- stack runghc --verbosity info --package pandoc-types-1.16.1 -}

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter removeNotes

removeNotes :: Inline -> Inline
removeNotes (Note _) = Str ""
removeNotes x = x

