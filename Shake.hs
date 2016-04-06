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
 ./Shake docs             # generate all docs
 ./Shake manpages         # generate nroff files for man
 ./Shake webmanpages      # generate web man pages for hakyll
 ./Shake m4manpages       # generate nroff files for man (alternate method)
 ./Shake m4webmanpages    # generate web man pages for hakyll (alternate method)
|]

buildDir = ".build"
pandoc =
  -- "stack exec -- pandoc" -- use the pandoc required above
  "pandoc"                  -- use pandoc in PATH (faster)
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
  | '_' `elem` p = "hledger-lib" </> "doc"
  | otherwise    = dropExtension p </> "doc"

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

    -- method 1:

    phony "docs" $ need [
       "manpages"
      ,"webmanpages"
      ]

    -- pandoc filters, these adjust master md files for web or man output
    phony "pandocfilters" $ need pandocFilters
    pandocFilters |%> \out -> do
      need [out <.> "hs"]
      cmd ("stack ghc") out

    -- man pages adjusted for man and converted to nroff, by pandoc
    let manpageNroffs = [manpageDir p </> p | p <- manpages]
    phony "manpages" $ need manpageNroffs
    manpageNroffs |%> \out -> do
      let md = out <.> "md"
          tmpl = "doc/manpage.nroff"
      need $ md : tmpl : pandocFilters
      cmd pandoc md "-s --template" tmpl "-o" out
        "--filter doc/pandoc-drop-web-blocks"
        "--filter doc/pandoc-drop-html-blocks"
        "--filter doc/pandoc-drop-html-inlines"
        "--filter doc/pandoc-drop-links"
        "--filter doc/pandoc-drop-notes"
        "--filter doc/pandoc-capitalize-headers"

    -- man pages adjusted for web by pandoc (ready for hakyll)
    let webManpageMds = ["site" </> p <.>".md" | p <- manpages]
    phony "webmanpages" $ need webManpageMds
    webManpageMds |%> \out -> do
      let p = dropExtension $ takeFileName out
          md = manpageDir p </> p <.> "md"
      need $ md : pandocFilters
      cmd pandoc md "-o" out "--filter doc/pandoc-drop-man-blocks"

    -- method 2:

    phony "m4docs" $ need [
       "m4manpages"
      ,"m4webmanpages"
      ]

    -- man pages assembled from parts and adjusted for man with m4, adjusted more and converted to nroff with pandoc
    let m4manpageNroffs = [manpageDir p </> "m4-"++p | p <- ["hledger.1"]]
    phony "m4manpages" $ need m4manpageNroffs
    m4manpageNroffs |%> \out -> do                      -- hledger/doc/m4-hledger.1
      let (dir,file) = splitFileName out                -- hledger/doc, m4-hledger.1
          m4src = dir </> drop 3 file <.> "md" <.> "m4" -- hledger/doc/hledger.1.md.m4
          m4includes = map (dir </>) ["description.md","examples.md","queries.md","commands.md","options.md"]
          m4lib = "doc/lib.m4"
          tmpl  = "doc/manpage.nroff"
      need $ m4src : m4lib : tmpl : pandocFilters ++ m4includes
      cmd Shell "m4 -P" "-DMAN" "-I" dir m4lib m4src "|" pandoc "-s --template" tmpl "-o" out
        "--filter doc/pandoc-drop-html-blocks"
        "--filter doc/pandoc-drop-html-inlines"
        "--filter doc/pandoc-drop-links"
        "--filter doc/pandoc-drop-notes"
        "--filter doc/pandoc-capitalize-headers"

    -- man pages assembled from parts and adjusted for web with m4, adjusted slightly more with pandoc (ready for hakyll)
    let m4webManpageMds = ["site" </> "m4-"++p <.>".md" | p <- ["hledger.1"]]
    phony "m4webmanpages" $ need $ m4webManpageMds
    m4webManpageMds |%> \out -> do                  -- site/m4-hledger.1.md
      let file  = takeFileName out                  -- m4-hledger.1.md
          manpage = drop 3 $ dropExtension file     -- hledger.1
          dir = manpageDir manpage                  -- hledger/doc
          m4src = dir </> manpage <.> "md" <.> "m4" -- hledger/doc/hledger.1.md.m4
          m4includes = map (dir </>) ["description.md","examples.md","queries.md","commands.md","options.md"]
          m4lib = "doc/lib.m4"
      need $ m4src : m4lib : m4includes
      cmd Shell "m4 -P" "-DWEB" "-I" dir m4lib m4src "|" pandoc "-o" out

    -- cleanup

    phony "clean" $ do
      putNormal "Cleaning generated files"
      -- removeFilesAfter "." manpageNroffs
      removeFilesAfter "." webManpageMds
      removeFilesAfter "." m4manpageNroffs
      removeFilesAfter "." $ map (<.> "md") m4manpageNroffs
      removeFilesAfter "." m4webManpageMds

    phony "Clean" $ do
      need ["clean"]
      putNormal "Cleaning object files"
      removeFilesAfter "doc" ["*.o","*.p_o","*.hi"] -- forces rebuild of exes ?
      putNormal "Cleaning shake build files"
      removeFilesAfter buildDir ["//*"]
