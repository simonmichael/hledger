{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , Specs
    ) where

import Yesod.Test

type Specs = SpecsConn ()
