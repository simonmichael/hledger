-- | Text.Tabular.AsciiArt from tabular-0.2.2.7, modified to treat
-- wide characters as double width.

module Text.Tabular.AsciiWide where

import Data.List (intersperse, transpose)
import Text.Tabular
import Hledger.Utils.String

-- | for simplicity, we assume that each cell is rendered
--   on a single line
render :: Bool -- ^ pretty tables
       -> (rh -> String)
       -> (ch -> String)
       -> (a -> String)
       -> Table rh ch a
       -> String
render pretty fr fc f (Table rh ch cells) =
  unlines $ [ bar SingleLine   -- +--------------------------------------+
            , renderColumns pretty sizes ch2
            , bar DoubleLine   -- +======================================+
            ] ++
            (renderRs $ fmap renderR $ zipHeader [] cells $ fmap fr rh) ++
            [ bar SingleLine ] -- +--------------------------------------+
 where
  bar = concat . renderHLine pretty sizes ch2
  -- ch2 and cell2 include the row and column labels
  ch2 = Group DoubleLine [Header "", fmap fc ch]
  cells2 = headerContents ch2
         : zipWith (\h cs -> h : map f cs) rhStrings cells
  --
  renderR (cs,h) = renderColumns pretty sizes $ Group DoubleLine
                    [ Header h
                    , fmap fst $ zipHeader "" (map f cs) ch]
  rhStrings = map fr $ headerContents rh
  -- maximum width for each column
  sizes   = map (maximum . map strWidth) . transpose $ cells2
  renderRs (Header s)   = [s]
  renderRs (Group p hs) = concat . intersperse sep . map renderRs $ hs
    where sep = renderHLine pretty sizes ch2 p

verticalBar :: Bool -> Char
verticalBar pretty = if pretty then '│' else '|'

leftBar :: Bool -> String
leftBar pretty = verticalBar pretty : " "

rightBar :: Bool -> String
rightBar pretty = " " ++ [verticalBar pretty]

midBar :: Bool -> String
midBar pretty = " " ++ verticalBar pretty : " "

doubleMidBar :: Bool -> String
doubleMidBar pretty = if pretty then " ║ " else " || "

horizontalBar :: Bool -> Char
horizontalBar pretty = if pretty then '─' else '-'

doubleHorizontalBar :: Bool -> Char
doubleHorizontalBar pretty = if pretty then '═' else '='

-- | We stop rendering on the shortest list!
renderColumns :: Bool -- ^ pretty
              -> [Int] -- ^ max width for each column
              -> Header String
              -> String
renderColumns pretty is h = leftBar pretty ++ coreLine ++ rightBar pretty
 where
  coreLine = concatMap helper $ flattenHeader $ zipHeader 0 is h
  helper = either hsep (uncurry padLeftWide)
  hsep :: Properties -> String
  hsep NoLine     = "  "
  hsep SingleLine = midBar pretty
  hsep DoubleLine = doubleMidBar pretty

renderHLine :: Bool -- ^ pretty
            -> [Int] -- ^ width specifications
            -> Header String
            -> Properties
            -> [String]
renderHLine _ _ _ NoLine = []
renderHLine pretty w h SingleLine = [renderHLine' pretty SingleLine w (horizontalBar pretty) h]
renderHLine pretty w h DoubleLine = [renderHLine' pretty DoubleLine w (doubleHorizontalBar pretty) h]

doubleCross :: Bool -> String
doubleCross pretty = if pretty then "╬" else "++"

doubleVerticalCross :: Bool -> String
doubleVerticalCross pretty = if pretty then "╫" else "++"

cross :: Bool -> Char
cross pretty = if pretty then '┼' else '+'

renderHLine' :: Bool -> Properties -> [Int] -> Char -> Header String -> String
renderHLine' pretty prop is sep h = [ cross pretty, sep ] ++ coreLine ++ [sep, cross pretty]
 where
  coreLine        = concatMap helper $ flattenHeader $ zipHeader 0 is h
  helper          = either vsep dashes
  dashes (i,_)    = replicate i sep
  vsep NoLine     = replicate 2 sep  -- match the double space sep in renderColumns 
  vsep SingleLine = sep : cross pretty : [sep]
  vsep DoubleLine = sep : cross' ++ [sep]
  cross' = case prop of
     DoubleLine -> doubleCross pretty
     _ -> doubleVerticalCross pretty

-- padLeft :: Int -> String -> String
-- padLeft l s = padding ++ s
--  where padding = replicate (l - length s) ' '

