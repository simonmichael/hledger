#!/usr/bin/env stack
{- stack runghc --verbosity info --package pandoc-types-1.16.1 -}
-- Ensure level 1 and 2 headings are first-letter-capitalised.

import Data.Char
import Text.Pandoc.JSON
import Text.Pandoc.Walk

main :: IO ()
main = toJSONFilter capitalizeHeaders

capitalizeHeaders :: Block -> Block
capitalizeHeaders (Header lvl attr xs) | lvl < 3 = Header lvl attr $ map capitalize (take 1 xs) ++ drop 1 xs
capitalizeHeaders x = x

capitalize :: Inline -> Inline
capitalize (Str s) = Str $ map toUpper (take 1 s) ++ map toLower (drop 1 s)
capitalize x = x

{-
capitalizeHeaderLinks :: Inline -> Inline
capitalizeHeaderLinks (Link xs t@('#':_,_)) = Link (walk capitalize xs) t
capitalizeHeaderLinks x = x
-}
