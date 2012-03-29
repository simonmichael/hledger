module Hledger.Web.Import
    ( module Prelude
    , (<>)
    , Text
    , module Data.Monoid
    , module Control.Applicative
    ) where

import Prelude hiding (writeFile, readFile, putStrLn)
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
