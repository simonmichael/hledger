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
    ) where

import Hledger.Data.Types (Amount)

import qualified Data.Text as T
import Data.Text (Text)


data Type =
      TypeString
    | TypeAmount !Amount
    | TypeMixedAmount
    deriving (Eq, Ord, Show)

data Style = Body Emphasis | Head
    deriving (Eq, Ord, Show)

data Emphasis = Item | Total
    deriving (Eq, Ord, Show)

data Cell =
    Cell {
        cellType :: Type,
        cellStyle :: Style,
        cellContent :: Text
    }

defaultCell :: Cell
defaultCell =
    Cell {
        cellType = TypeString,
        cellStyle = Body Item,
        cellContent = T.empty
    }
