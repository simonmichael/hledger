{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , module Foundation
    , Specs
    ) where

import Yesod.Test

import Foundation

type Specs = YesodSpec App
