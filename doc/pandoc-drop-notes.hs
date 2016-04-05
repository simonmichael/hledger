#!/usr/bin/env stack
{- stack runghc --verbosity info --package pandoc-types-1.16.1 -}

import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter dropNotes

dropNotes :: Inline -> Inline
dropNotes (Note _) = Str ""
dropNotes x = x

