{- |
Rich data type to describe data in a table.
This is the basis for ODS and HTML export.
-}
module Hledger.Write.Spreadsheet (
    Type(..),
    Style(..),
    Emphasis(..),
    Cell(..),
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

data Cell text =
    Cell {
        cellType :: Type,
        cellStyle :: Style,
        cellContent :: text
    }

instance Functor Cell where
    fmap f (Cell typ style content) = Cell typ style $ f content

defaultCell :: text -> Cell text
defaultCell text =
    Cell {
        cellType = TypeString,
        cellStyle = Body Item,
        cellContent = text
    }

emptyCell :: (Monoid text) => Cell text
emptyCell = defaultCell mempty
