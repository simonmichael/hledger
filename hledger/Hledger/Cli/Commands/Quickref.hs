{-|

The quick reference card: a compact overview of hledger's most useful
commands, flags, and concepts, shown by @hledger help quickref@.

-}

{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Quickref (
  showQuickref
) where

import System.Console.ANSI (setSGRCode, SGR(..), Underlining(..))

import Hledger

-- | The quick reference card, from quickref.txt in the hledger package directory.
quickrefcontent :: String
quickrefcontent = $(embedFileRelative "quickref.txt")

-- | Print the quick reference card, styling it a little when color is enabled.
showQuickref :: IO ()
showQuickref = do
  usecolor <- useColorOnStdout
  runPager $ (if usecolor then styleQuickref else id) quickrefcontent

-- | Style the card: the title line gets hledger's usual bold colour gradient,
-- section headings (lines with nothing beyond the label margin) are faint and
-- underlined, other labels in the margin (the first 16 columns) are faint,
-- and the commands, flags etc. beyond them are left normal,
-- except parentheses, which are faint.
styleQuickref :: String -> String
styleQuickref s = unlines $ case lines s of
  []         -> []
  title:rest -> gradientStr bold' 1 (length title) 0 0 title : map styleline rest
  where
    labelwidth = 16
    styleline l
      | all (==' ') label = label <> dimparens content
      | null content      = underline' $ faint' l
      | otherwise         = faint' label <> dimparens content
      where (label, content) = splitAt labelwidth l
    dimparens = concatMap (\c -> if c `elem` "()" then faint' [c] else [c])

-- | Wrap a string in ANSI codes to switch single underlining on and off.
underline' :: String -> String
underline' s = setSGRCode [SetUnderlining SingleUnderline] <> s <> setSGRCode [SetUnderlining NoUnderline]
