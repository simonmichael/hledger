{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import Yesod.Default.Config
import Yesod.Test
import Test.Hspec (hspec)
import Application (makeFoundation)
import Hledger.Web.Options (defwebopts)

import HomeTest

main :: IO ()
main = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
                    { csParseExtra = parseExtra
                    }
    foundation <- makeFoundation conf defwebopts
    hspec $ do
      yesodSpec foundation $ do
        homeSpecs
