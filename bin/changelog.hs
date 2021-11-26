#!/usr/bin/env stack
{- stack script --resolver nightly-2021-11-19
    --package data-default
    --package extra
    --package hledger-lib
    --package process
    --package text
-}
{- stack ghc
    --package text
-}
-- changelog.hs CHANGELOGFILE
--
-- Manipulate a hledger changelog. Currently does one thing: prompts
-- for a rewrite of the oldest uncategorised pending changelog item
-- and updates the file, printing a diff.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char
import Data.Default
import GHC.Generics
import Data.List.Extra
import qualified Data.Text as T
import System.Environment
import System.IO
import System.Process
import Text.Printf
import Hledger.Utils

-- The one-line heading for a top level section in the changelog,
-- with the leading #(s) removed.
type ChangelogHeading = String

-- One change description in the changelog, with the list bullet and
-- corresponding indentation removed.
type ChangelogItem = String

-- A top level section in the changelog, corresponding to one release.
data ChangelogSection = ChangelogSection {
   heading          :: ChangelogHeading
  ,unknownitems     :: [ChangelogItem]
  ,featureitems     :: [ChangelogItem]
  ,improvementitems :: [ChangelogItem]
  ,fixitems         :: [ChangelogItem]
  }
  deriving (Show, Generic, Default)

main = do
  (i:_) <- getArgs

  -- read specified changelog
  (preamble:first:rest) <- splitOn "\n# " <$> readFile i
  let
    o = i ++ ".new"
    s@ChangelogSection{..} = readSection first
  s' <- editOneUnknown s

  -- write back to new file
  writeFile o $ init $ unlines $
     preamble :
     showSection s' :
     map ("# "++) rest

  -- show the diff
  system' $ printf "diff %s %s" i o

  -- overwrite the old file
  system' $ printf "mv %s %s" o i

editOneUnknown :: ChangelogSection -> IO ChangelogSection
editOneUnknown s@ChangelogSection{..}
  | null unknownitems = return s
  | otherwise = do
    let u = last unknownitems
    putStrLn "Old:\n"
    putStrLn u
    putStrLn "New: (prefix with feat:/imp:/fix: to categorise, ctrl-d to finish):\n" -- Just an = keeps it unchanged, empty string removes it.\n"
    i <- getContents
    let s' =
          case i of
            'f':'e':'a':'t':':':' ':t -> s{featureitems     = readItem t : featureitems}
            'f':'i':'x':':':' ':t     -> s{fixitems         = readItem t : fixitems}
            'i':'m':'p':':':' ':t     -> s{improvementitems = readItem t : improvementitems}
            t                         -> s{improvementitems = readItem t : improvementitems}
    return s'{unknownitems=init unknownitems}

-- Parse a changelog section which may or may not have the Features/Improvements/Fixes subheadings.
readSection :: String -> ChangelogSection
readSection s =
  let
    (heading,rest) = break (=='\n') s
    parts = splitOn "\nFeatures\n" rest
    (unknownitems, featureitems, improvementitems, fixitems) =
      case parts of
        []       -> ([], [], [], [])
        [u]      -> (readItems u, [], [], [])
        (u:xs:_) -> (readItems u, readItems f, readItems i, readItems x)
          where
            (f:ys:_) = splitOn "\nImprovements\n" xs
            (i:x:_)  = splitOn "\nFixes\n" ys
  in ChangelogSection{..}
  where
    readItems = map readItem . filter (not.all isSpace) . splitOn "\n- "

showSection ChangelogSection{..} =
  unlines $
       [("# "++heading), ""]
    ++ map showItem unknownitems
    ++ ["Features", ""]
    ++ map showItem featureitems
    ++ ["Improvements", ""]
    ++ map showItem improvementitems
    ++ ["Fixes", ""]
    ++ map showItem fixitems

readItem :: String -> ChangelogItem
readItem "" = def
readItem s =
  let
    (first:rest) = lines s
    stripto2spaces (' ':' ':t) = t
    stripto2spaces (' ':t) = t
    stripto2spaces t = t
  in unlines $
     first :
     map stripto2spaces rest

showItem "" = ""
showItem i =
  let (first:rest) = lines i
  in unlines $ ("- "++first) : map ("  "++) rest

system' s = putStrLn s >> system s

re = toRegex' . T.pack

