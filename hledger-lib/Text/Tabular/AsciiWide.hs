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
  unlines $ [ bar VT SingleLine   -- +--------------------------------------+
            , renderColumns pretty sizes ch2
            , bar VM DoubleLine   -- +======================================+
            ] ++
            (renderRs $ fmap renderR $ zipHeader [] cells $ fmap fr rh) ++
            [ bar VB SingleLine ] -- +--------------------------------------+
 where
  bar vpos prop = concat (renderHLine vpos pretty sizes ch2 prop)
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
    where sep = renderHLine VM pretty sizes ch2 p

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

renderHLine :: VPos
            -> Bool -- ^ pretty
            -> [Int] -- ^ width specifications
            -> Header String
            -> Properties
            -> [String]
renderHLine _ _ _ _ NoLine = []
renderHLine vpos pretty w h prop = [renderHLine' vpos pretty prop w h]

renderHLine' :: VPos -> Bool -> Properties -> [Int] -> Header String -> String
renderHLine' vpos pretty prop is h = edge HL ++ sep ++ coreLine ++ sep ++ edge HR
 where
  edge hpos       = boxchar vpos hpos SingleLine prop pretty
  coreLine        = concatMap helper $ flattenHeader $ zipHeader 0 is h
  helper          = either vsep dashes
  dashes (i,_)    = concat (replicate i sep)
  sep             = boxchar vpos HM NoLine prop pretty
  vsep v          = case v of
                      NoLine -> sep ++ sep
                      _      -> sep ++ cross v prop ++ sep
  cross v h       = boxchar vpos HM v h pretty

data VPos = VT | VM | VB -- top middle bottom
data HPos = HL | HM | HR -- left middle right

boxchar :: VPos -> HPos -> Properties -> Properties -> Bool -> String
boxchar vpos hpos vert horiz = lineart u d l r
  where
    u =
      case vpos of
        VT -> NoLine
        _  -> vert
    d =
      case vpos of
        VB -> NoLine
        _  -> vert
    l =
      case hpos of
        HL -> NoLine
        _  -> horiz
    r =
      case hpos of
        HR -> NoLine
        _  -> horiz

pick :: String -> String -> Bool -> String
pick x _ True  = x
pick _ x False = x

lineart :: Properties -> Properties -> Properties -> Properties -> Bool -> String
--      up         down       left      right
lineart SingleLine SingleLine SingleLine SingleLine = pick "┼" "+"
lineart SingleLine SingleLine SingleLine NoLine     = pick "┤" "+"
lineart SingleLine SingleLine NoLine     SingleLine = pick "├" "+"
lineart SingleLine NoLine     SingleLine SingleLine = pick "┴" "+"
lineart NoLine     SingleLine SingleLine SingleLine = pick "┬" "+"
lineart SingleLine NoLine     NoLine     SingleLine = pick "└" "+"
lineart SingleLine NoLine     SingleLine NoLine     = pick "┘" "+"
lineart NoLine     SingleLine SingleLine NoLine     = pick "┐" "+"
lineart NoLine     SingleLine NoLine     SingleLine = pick "┌" "+"
lineart SingleLine SingleLine NoLine     NoLine     = pick "│" "|"
lineart NoLine     NoLine     SingleLine SingleLine = pick "─" "-"

lineart DoubleLine DoubleLine DoubleLine DoubleLine = pick "╬" "++"
lineart DoubleLine DoubleLine DoubleLine NoLine     = pick "╣" "++"
lineart DoubleLine DoubleLine NoLine     DoubleLine = pick "╠" "++"
lineart DoubleLine NoLine     DoubleLine DoubleLine = pick "╩" "++"
lineart NoLine     DoubleLine DoubleLine DoubleLine = pick "╦" "++"
lineart DoubleLine NoLine     NoLine     DoubleLine = pick "╚" "++"
lineart DoubleLine NoLine     DoubleLine NoLine     = pick "╝" "++"
lineart NoLine     DoubleLine DoubleLine NoLine     = pick "╗" "++"
lineart NoLine     DoubleLine NoLine     DoubleLine = pick "╔" "++"
lineart DoubleLine DoubleLine NoLine     NoLine     = pick "║" "||"
lineart NoLine     NoLine     DoubleLine DoubleLine = pick "═" "="

lineart DoubleLine NoLine     NoLine     SingleLine = pick "╙" "++"
lineart DoubleLine NoLine     SingleLine NoLine     = pick "╜" "++"
lineart NoLine     DoubleLine SingleLine NoLine     = pick "╖" "++"
lineart NoLine     DoubleLine NoLine     SingleLine = pick "╓" "++"

lineart SingleLine NoLine     NoLine     DoubleLine = pick "╘" "+"
lineart SingleLine NoLine     DoubleLine NoLine     = pick "╛" "+"
lineart NoLine     SingleLine DoubleLine NoLine     = pick "╕" "+"
lineart NoLine     SingleLine NoLine     DoubleLine = pick "╒" "+"

lineart DoubleLine DoubleLine SingleLine NoLine     = pick "╢" "++"
lineart DoubleLine DoubleLine NoLine     SingleLine = pick "╟" "++"
lineart DoubleLine NoLine     SingleLine SingleLine = pick "╨" "++"
lineart NoLine     DoubleLine SingleLine SingleLine = pick "╥" "++"

lineart SingleLine SingleLine DoubleLine NoLine     = pick "╡" "+"
lineart SingleLine SingleLine NoLine     DoubleLine = pick "╞" "+"
lineart SingleLine NoLine     DoubleLine DoubleLine = pick "╧" "+"
lineart NoLine     SingleLine DoubleLine DoubleLine = pick "╤" "+"

lineart SingleLine SingleLine DoubleLine DoubleLine = pick "╪" "+"
lineart DoubleLine DoubleLine SingleLine SingleLine = pick "╫" "++"

lineart _          _          _          _          = const ""

-- 
