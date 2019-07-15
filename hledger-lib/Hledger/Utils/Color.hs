-- | Basic color helpers for prettifying console output.

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Utils.Color
(
  color,
  bgColor,
  Color(..),
  ColorIntensity(..)
)
where

import System.Console.ANSI


-- | Wrap a string in ANSI codes to set and reset foreground colour.
color :: ColorIntensity -> Color -> String -> String
color int col s = setSGRCode [SetColor Foreground int col] ++ s ++ setSGRCode []

-- | Wrap a string in ANSI codes to set and reset background colour.
bgColor :: ColorIntensity -> Color -> String -> String
bgColor int col s = setSGRCode [SetColor Background int col] ++ s ++ setSGRCode []
