{-# LANGUAGE CPP #-}
module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative  as Import (pure, (<$>), (<*>))
#endif
import           Data.Text            as Import (Text)

import           Foundation           as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import (
#if !MIN_VERSION_base(4,8,0)
                                                 Monoid (mappend, mempty, mconcat),
#endif
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
