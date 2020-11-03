-- | Text.Tabular.AsciiArt from tabular-0.2.2.7, modified to treat
-- wide characters as double width.

module Text.Tabular.AsciiWide where

import Data.Maybe (fromMaybe)
import Data.Default (Default(..))
import Data.List (intersperse, transpose)
import Safe (maximumMay)
import Text.Tabular
import Text.WideString (strWidth)


-- | The options to use for rendering a table.
data TableOpts = TableOpts
  { prettyTable  :: Bool  -- ^ Pretty tables
  , tableBorders :: Bool  -- ^ Whether to display the outer borders
  , borderSpaces :: Bool  -- ^ Whether to display spaces around bars
  } deriving (Show)

instance Default TableOpts where
  def = TableOpts { prettyTable  = False
                  , tableBorders = True
                  , borderSpaces = True
                  }

-- | Cell contents along an alignment
data Cell = Cell Align [(String, Int)]
    deriving (Show)

-- | How to align text in a cell
data Align = TopRight | BottomRight | BottomLeft | TopLeft
  deriving (Show)

emptyCell :: Cell
emptyCell = Cell TopRight []

-- | Create a single-line cell from the given contents with its natural width.
alignCell :: Align -> String -> Cell
alignCell a x = Cell a [(x, strWidth x)]

-- | Return the width of a Cell.
cellWidth :: Cell -> Int
cellWidth (Cell _ xs) = fromMaybe 0 . maximumMay $ map snd xs


-- | Render a table according to common options, for backwards compatibility
render :: Bool -> (rh -> String) -> (ch -> String) -> (a -> String) -> Table rh ch a -> String
render pretty fr fc f = renderTable def{prettyTable=pretty} (cell . fr) (cell . fc) (cell . f)
  where cell = alignCell TopRight

-- | Render a table according to various cell specifications
renderTable :: TableOpts         -- ^ Options controlling Table rendering
            -> (rh -> Cell)  -- ^ Rendering function for row headers
            -> (ch -> Cell)  -- ^ Rendering function for column headers
            -> (a -> Cell)   -- ^ Function determining the string and width of a cell
            -> Table rh ch a
            -> String
renderTable topts@TableOpts{prettyTable=pretty, tableBorders=borders} fr fc f (Table rh ch cells) =
  unlines . addBorders $
    renderColumns topts sizes ch2
    : bar VM DoubleLine   -- +======================================+
    : renderRs (fmap renderR $ zipHeader [] cellContents rowHeaders)
 where
  renderR (cs,h) = renderColumns topts sizes $ Group DoubleLine
                     [ Header h
                     , fmap fst $ zipHeader emptyCell cs colHeaders
                     ]

  rowHeaders   = fmap fr rh
  colHeaders   = fmap fc ch
  cellContents = map (map f) cells

  -- ch2 and cell2 include the row and column labels
  ch2 = Group DoubleLine [Header emptyCell, colHeaders]
  cells2 = headerContents ch2 : zipWith (:) (headerContents rowHeaders) cellContents

  -- maximum width for each column
  sizes = map (fromMaybe 0 . maximumMay . map cellWidth) $ transpose cells2
  renderRs (Header s)   = [s]
  renderRs (Group p hs) = concat . intersperse sep $ map renderRs hs
    where sep = renderHLine VM borders pretty sizes ch2 p

  -- borders and bars
  addBorders xs = if borders then bar VT SingleLine : xs ++ [bar VB SingleLine] else xs
  bar vpos prop = concat $ renderHLine vpos borders pretty sizes ch2 prop

-- | Render a single row according to cell specifications.
renderRow :: TableOpts -> Header Cell -> String
renderRow topts h = renderColumns topts is h
  where is = map (\(Cell _ xs) -> fromMaybe 0 . maximumMay $ map snd xs) $ headerContents h


verticalBar :: Bool -> Char
verticalBar pretty = if pretty then '│' else '|'

leftBar :: Bool -> Bool -> String
leftBar pretty True  = verticalBar pretty : " "
leftBar pretty False = [verticalBar pretty]

rightBar :: Bool -> Bool -> String
rightBar pretty True  = ' ' : [verticalBar pretty]
rightBar pretty False = [verticalBar pretty]

midBar :: Bool -> Bool -> String
midBar pretty True  = ' ' : verticalBar pretty : " "
midBar pretty False = [verticalBar pretty]

doubleMidBar :: Bool -> Bool -> String
doubleMidBar pretty True  = if pretty then " ║ " else " || "
doubleMidBar pretty False = if pretty then "║" else "||"

-- | We stop rendering on the shortest list!
renderColumns :: TableOpts  -- ^ rendering options for the table
              -> [Int]      -- ^ max width for each column
              -> Header Cell
              -> String
renderColumns TableOpts{prettyTable=pretty, tableBorders=borders, borderSpaces=spaces} is h =
    concat . intersperse "\n"                    -- Put each line on its own line
    . map (addBorders . concat) . transpose      -- Change to a list of lines and add borders
    . map (either hsep padCell) . flattenHeader  -- We now have a matrix of strings
    . zipHeader 0 is $ padRow <$> h  -- Pad cell height and add width marker
  where
    -- Pad each cell to have the appropriate width
    padCell (w, Cell TopLeft     ls) = map (\(x,xw) -> x ++ replicate (w - xw) ' ') ls
    padCell (w, Cell BottomLeft  ls) = map (\(x,xw) -> x ++ replicate (w - xw) ' ') ls
    padCell (w, Cell TopRight    ls) = map (\(x,xw) -> replicate (w - xw) ' ' ++ x) ls
    padCell (w, Cell BottomRight ls) = map (\(x,xw) -> replicate (w - xw) ' ' ++ x) ls

    -- Pad each cell to have the same number of lines
    padRow (Cell TopLeft     ls) = Cell TopLeft     $ ls ++ replicate (nLines - length ls) ("",0)
    padRow (Cell TopRight    ls) = Cell TopRight    $ ls ++ replicate (nLines - length ls) ("",0)
    padRow (Cell BottomLeft  ls) = Cell BottomLeft  $ replicate (nLines - length ls) ("",0) ++ ls
    padRow (Cell BottomRight ls) = Cell BottomRight $ replicate (nLines - length ls) ("",0) ++ ls

    hsep :: Properties -> [String]
    hsep NoLine     = replicate nLines $ if spaces then "  " else ""
    hsep SingleLine = replicate nLines $ midBar pretty spaces
    hsep DoubleLine = replicate nLines $ doubleMidBar pretty spaces

    addBorders xs | borders   = leftBar pretty spaces ++ xs ++ rightBar pretty spaces
                  | spaces    =  ' ' : xs ++ " "
                  | otherwise = xs

    nLines = fromMaybe 0 . maximumMay . map (\(Cell _ ls) -> length ls) $ headerContents h

renderHLine :: VPos
            -> Bool  -- ^ show outer borders
            -> Bool -- ^ pretty
            -> [Int] -- ^ width specifications
            -> Header a
            -> Properties
            -> [String]
renderHLine _ _ _ _ _ NoLine = []
renderHLine vpos borders pretty w h prop = [renderHLine' vpos borders pretty prop w h]

renderHLine' :: VPos -> Bool -> Bool -> Properties -> [Int] -> Header a -> String
renderHLine' vpos borders pretty prop is h = addBorders $ sep ++ coreLine ++ sep
 where
  addBorders xs   = if borders then edge HL ++ xs ++ edge HR else xs
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
