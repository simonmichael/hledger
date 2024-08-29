{- |
Rich data type to describe data in a table.
This is the basis for ODS and HTML export.
-}
module Hledger.Write.Spreadsheet (
    Type(..),
    Style(..),
    Emphasis(..),
    Cell(..),
    Border(..),
    Lines(..),
    NumLines(..),
    noBorder,
    defaultCell,
    emptyCell,
    ) where

import Hledger.Data.Types (Amount)


data Type =
      TypeString
    | TypeAmount !Amount
    | TypeMixedAmount
    | TypeDate
    deriving (Eq, Ord, Show)

data Style = Body Emphasis | Head
    deriving (Eq, Ord, Show)

data Emphasis = Item | Total
    deriving (Eq, Ord, Show)


class Lines border where noLine :: border
instance Lines () where noLine = ()
instance Lines NumLines where noLine = NoLine

{- |
The same as Tab.Properties, but has 'Eq' and 'Ord' instances.
We need those for storing 'NumLines' in 'Set's.
-}
data NumLines = NoLine | SingleLine | DoubleLine
    deriving (Eq, Ord, Show)

data Border lines =
    Border {
        borderLeft, borderRight,
        borderTop, borderBottom :: lines
    }
    deriving (Eq, Ord, Show)

instance Functor Border where
    fmap f (Border left right top bottom) =
        Border (f left) (f right) (f top) (f bottom)

instance Applicative Border where
    pure a = Border a a a a
    Border fLeft fRight fTop fBottom <*> Border left right top bottom =
        Border (fLeft left) (fRight right) (fTop top) (fBottom bottom)

instance Foldable Border where
    foldMap f (Border left right top bottom) =
        f left <> f right <> f top <> f bottom

noBorder :: (Lines border) => Border border
noBorder = pure noLine


data Cell border text =
    Cell {
        cellType :: Type,
        cellBorder :: Border border,
        cellStyle :: Style,
        cellContent :: text
    }

instance Functor (Cell border) where
    fmap f (Cell typ border style content) =
        Cell typ border style $ f content

defaultCell :: (Lines border) => text -> Cell border text
defaultCell text =
    Cell {
        cellType = TypeString,
        cellBorder = noBorder,
        cellStyle = Body Item,
        cellContent = text
    }

emptyCell :: (Lines border, Monoid text) => Cell border text
emptyCell = defaultCell mempty
