#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package pandoc
-}
-- Remove a table of contents marker
-- (a bullet list item containing "toc[-N[-M]]")

{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.List.Split
import Data.Maybe
import Safe
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter dropToc

dropHtmlBlocks :: Block -> Block
dropHtmlBlocks (RawBlock (Format "html") _) = Plain []
dropHtmlBlocks x = x

 -- BulletList
 --  [ [Plain [Str "toc"]] ]
dropToc :: Block -> Block
dropToc (BulletList is) =
  BulletList $ filter (not.null) $ map (filter isNotToc) is
  where
    isNotToc (Plain [Str s]) | isJust $ tocParams s = False
    isNotToc _ = True
dropToc x = x

tocParams :: String -> Maybe (Maybe Int, Maybe Int)
tocParams s =
  case splitOn "-" s of
  ["toc"]                                    -> Just (Nothing, Nothing)
  ["toc",a]   | all isDigit a                -> Just (Nothing, readMay a)
  ["toc",a,b] | all isDigit a, all isDigit b -> Just (readMay a, readMay b)
  _                                          -> Nothing

