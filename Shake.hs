#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package base-prelude
   --package directory
   --package extra
   --package here
   --package safe
   --package shake
   --package time
   --package pandoc
-}
{-
Usage: see below.
Shake.hs is a more powerful Makefile, providing a number of commands
for performing useful tasks. Compiling this script is suggested, so that
it runs quicker and will not be affected eg when exploring old code versions.
More about Shake: http://shakebuild.com/manual
Requires: https://www.haskell.org/downloads#stack

Shake notes:
notes:
 unclear:
  oracles
 wishlist:
  wildcards in phony rules
  multiple individually accessible wildcards
  just one shake import
-}

{-# LANGUAGE PackageImports, QuasiQuotes #-}

import                Prelude ()
import "base-prelude" BasePrelude
-- import "base"         System.Console.GetOpt
import "extra"        Data.List.Extra
import "here"         Data.String.Here
import "safe"         Safe
import "shake"        Development.Shake
import "shake"        Development.Shake.FilePath
import "time"         Data.Time
import "directory"    System.Directory as S (getDirectoryContents)

usage = [i|Usage:
 ./Shake.hs compile       # compile this script (optional)
 ./Shake --help           # show options, eg --color
 ./Shake                  # show commands
 ./Shake manpages         # generate nroff files for man
 ./Shake webmanpages      # generate web man pages for hakyll
|]

buildDir = ".build"
pandocExe = "stack exec -- pandoc" -- use the pandoc required above
pandocFiltersResolver = ""
manpages = [
   "hledger_csv.5"
  ,"hledger_journal.5"
  ,"hledger_timedot.5"
  ,"hledger_timelog.5"
  ,"hledger.1"
  ,"hledger-api.1"
  ,"hledger-ui.1"
  ,"hledger-web.1"
  ]

manpageDir p
  | '_' `elem` p = "hledger-lib"
  | otherwise    = dropExtension p

main = do

  pandocFilters <-
    map ("doc" </>). nub . sort . map (-<.> "") . filter ("pandoc-" `isPrefixOf`)
    <$> S.getDirectoryContents "doc"

  shakeArgs
    shakeOptions{
       shakeFiles=buildDir
      ,shakeVerbosity=Loud
      -- ,shakeReport=[".shake.html"]
      } $ do

    want ["help"]

    phony "help" $ liftIO $ putStrLn usage

    phony "compile" $ need ["Shake"]

    "Shake" %> \out -> do
      need ["Shake.hs"]
      cmd "stack ghc Shake.hs" :: Action ExitCode
      putLoud "Compiled ./Shake, you can now use this instead of ./Shake.hs"

    -- docs

    -- man pages, converted to man nroff with web-only sections removed
    let manpageNroffs = [manpageDir p </> p | p <- manpages]

    -- man pages, still markdown but with man-only sections removed
    -- (we let hakyll do the final markdown rendering)
    let webManpageMds = ["site" </> p <.>".md" | p <- manpages]

    phony "manpages" $ need manpageNroffs

    manpageNroffs |%> \out -> do
      let
        md = out <.> "md"
        tmpl = "doc/manpage.nroff"
      need $ md : tmpl : pandocFilters
      cmd pandocExe md "--to man -s --template" tmpl
        "--filter doc/pandoc-drop-html-blocks"
        "--filter doc/pandoc-drop-html-inlines"
        "--filter doc/pandoc-drop-links"
        "--filter doc/pandoc-drop-notes"
        "--filter doc/pandoc-capitalize-headers"
        "-o" out

    phony "webmanpages" $ need webManpageMds

    webManpageMds |%> \out -> do
      let
        p = dropExtension $ takeFileName out
        md = manpageDir p </> p <.> "md"
        tmpl = "doc/manpage.html"
      need $ md : tmpl : pandocFilters
      cmd pandocExe md "--to markdown"
        "--filter doc/pandoc-drop-man-blocks"
        "-o" out

    phony "pandocfilters" $ need pandocFilters

    pandocFilters |%> \out -> do
      need [out <.> "hs"]
      cmd ("stack "++pandocFiltersResolver++" ghc") out

    -- cleanup

    phony "clean" $ do
      putNormal "Cleaning generated files"
      removeFilesAfter "" manpageNroffs
      removeFilesAfter "" webManpageMds
      putNormal "Cleaning object files"
      removeFilesAfter "tools" ["*.o","*.p_o","*.hi"]
      putNormal "Cleaning shake build files"
      removeFilesAfter buildDir ["//*"]

