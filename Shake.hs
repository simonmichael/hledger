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
  just one shake import
  wildcards in phony rules
  multiple individually accessible wildcards
  not having to write :: Action ExitCode after a non-final cmd
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
 ./Shake site             # generate things needed for the website
 ./Shake manpages         # generate nroff files for man
 ./Shake webmanpages      # generate web man pages for hakyll
 ./Shake webmanual        # generate combined web man page for hakyll
 ./Shake m4manpages       # generate nroff files for man (alternate method)
 ./Shake m4webmanpages    # generate web man pages for hakyll (alternate method)
|]

buildDir = ".build"
pandoc =
  -- "stack exec -- pandoc" -- use the pandoc required above
  "pandoc"                  -- use pandoc in PATH (faster)

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

    phony "site" $ need [
       "manpages"
      ,"webmanpages"
      ,"m4manpages"
      ,"m4webmanpages"
      ,"site/manual2.md"
      ]

    -- man pages

    let
      manpageNames = [ -- in suggested reading order
         "hledger.1"
        ,"hledger-ui.1"
        ,"hledger-web.1"
        ,"hledger-api.1"
        ,"hledger_journal.5"
        ,"hledger_csv.5"
        ,"hledger_timelog.5"
        ,"hledger_timedot.5"
        ]

      -- hledger.1 -> hledger, hledger_journal.5 -> journal
      manpageNameToUri m | "hledger_" `isPrefixOf` m = dropExtension $ drop 8 m
                         | otherwise                 = dropExtension m

      -- hledger -> hledger.1, journal -> hledger_journal.5
      manpageUriToName u | "hledger" `isPrefixOf` u = u <.> "1"
                         | otherwise                = "hledger_" ++ u <.> "5"

      -- hledger.1 -> hledger/doc, hledger_journal.5 -> hledger-lib/doc
      manpageDir m
        | '_' `elem` m = "hledger-lib" </> "doc"
        | otherwise    = dropExtension m </> "doc"

    -- method 1:

    -- pandoc filters, these adjust master md files for web or man output
    phony "pandocfilters" $ need pandocFilters
    pandocFilters |%> \out -> do
      need [out <.> "hs"]
      cmd ("stack ghc") out

    -- man pages adjusted for man and converted to nroff, by pandoc
    let manpages = [manpageDir m </> m | m <- manpageNames] -- hledger/doc/hledger.1, hledger-lib/doc/hledger_journal.5
    phony "manpages" $ need manpages
    manpages |%> \out -> do
      let md = out <.> "md"  -- hledger/doc/hledger.1.md
          tmpl = "doc/manpage.nroff"
      need $ md : tmpl : pandocFilters
      cmd pandoc md "-s --template" tmpl
        "--filter doc/pandoc-drop-web-blocks"
        "--filter doc/pandoc-drop-html-blocks"
        "--filter doc/pandoc-drop-html-inlines"
        "--filter doc/pandoc-drop-links"
        "--filter doc/pandoc-drop-notes"
        "-o" out

    -- man pages adjusted for web by pandoc (ready for hakyll)
    let webmanpages = ["site" </> manpageNameToUri m <.>".md" | m <- manpageNames] -- site/hledger.md, site/journal.md
    phony "webmanpages" $ need webmanpages
    webmanpages |%> \out -> do
      let m = manpageUriToName $ dropExtension $ takeFileName out  -- hledger.1
          md = manpageDir m </> m <.> "md"                         -- hledger/doc/hledger.1.md
      need $ md : pandocFilters
      cmd pandoc md "--atx-headers"
        "--filter doc/pandoc-demote-headers"
        -- "--filter doc/pandoc-add-toc"
        -- "--filter doc/pandoc-drop-man-blocks"
        "-o" out

    -- method 2:

    -- man pages assembled from parts and adjusted for man with m4, adjusted more and converted to nroff with pandoc
    let m4manpages = [manpageDir m </> "m4-"++m | m <- ["hledger.1"]] -- hledger/doc/m4-hledger.1
    phony "m4manpages" $ need m4manpages
    m4manpages |%> \out -> do                           -- hledger/doc/m4-hledger.1
      let (dir,file) = splitFileName out                -- hledger/doc, m4-hledger.1
          m4src = dir </> drop 3 file <.> "md" <.> "m4" -- hledger/doc/hledger.1.md.m4
          m4includes = map (dir </>) ["description.md","examples.md","queries.md","commands.md","options.md"]
          m4lib = "doc/lib.m4"
          tmpl  = "doc/manpage.nroff"
      need $ m4src : m4lib : tmpl : pandocFilters ++ m4includes
      cmd Shell "m4 -P" "-DMAN" "-I" dir m4lib m4src
        "|" pandoc "-s --template" tmpl
        "--filter doc/pandoc-drop-html-blocks"
        "--filter doc/pandoc-drop-html-inlines"
        "--filter doc/pandoc-drop-links"
        "--filter doc/pandoc-drop-notes"
        "-o" out

    -- man pages assembled from parts and adjusted for web with m4, adjusted slightly more with pandoc (ready for hakyll)
    let m4webmanpages = ["site" </> "m4-" ++ manpageNameToUri m <.> ".md" | m <- ["hledger.1"]] -- site/m4-hledger.md
    phony "m4webmanpages" $ need $ m4webmanpages
    m4webmanpages |%> \out -> do                                   -- site/m4-hledger.md
      let file  = takeFileName out                                 -- m4-hledger.1.md
          manpage = manpageUriToName $ drop 3 $ dropExtension file -- hledger.1
          dir = manpageDir manpage                                 -- hledger/doc
          m4src = dir </> manpage <.> "md" <.> "m4"                -- hledger/doc/hledger.1.md.m4
          m4includes = map (dir </>) ["description.md","examples.md","queries.md","commands.md","options.md"]
          m4lib = "doc/lib.m4"
      need $ m4src : m4lib : m4includes
      cmd Shell "m4 -P" "-DMAN -DWEB" "-I" dir m4lib m4src
        "|" pandoc
        "--filter doc/pandoc-demote-headers"
        -- "--filter doc/pandoc-add-toc"
        "-o" out

    -- web manual combined from man pages

    let webmanual = "site/manual2.md"
    phony "webmanual" $ need [ webmanual ]
    "site/manual2.md" %> \out -> do
      need webmanpages
      liftIO $ writeFile webmanual [i|
<style>
#toc > ol > li {
  padding-top:1em;
  font-weight:bold;
}
#toc > ol > li > ol {
  font-weight:normal;
}
</style>
* toc

|]
      forM_ webmanpages $ \f -> do -- site/hledger.md, site/journal.md
        let heading =
              let h = dropExtension $ takeFileName f -- hledger, journal
              in if "hledger" `isPrefixOf` h
                 then h                              -- hledger
                 else h ++ " format"                 -- journal format
        cmd Shell ("printf '\\n## "++ heading ++"\\n\\n' >>") webmanual :: Action ExitCode
        cmd Shell "pandoc" f "-t markdown --atx-headers"
          "--filter doc/pandoc-drop-man-blocks"
          "--filter doc/pandoc-drop-toc"
          -- "--filter doc/pandoc-capitalize-headers"
          "--filter doc/pandoc-demote-headers"
          ">>" webmanual :: Action ExitCode

    -- cleanup

    phony "clean" $ do
      putNormal "Cleaning generated files"
      removeFilesAfter "." m4manpages
      removeFilesAfter "." m4webmanpages
      removeFilesAfter "." webmanpages
      removeFilesAfter "." [webmanual]

    phony "Clean" $ do
      need ["clean"]
      putNormal "Cleaning generated man page nroffs"
      removeFilesAfter "." manpages
      putNormal "Cleaning object files"
      removeFilesAfter "doc" ["*.o","*.p_o","*.hi"] -- forces rebuild of exes ?
      putNormal "Cleaning shake build files"
      removeFilesAfter buildDir ["//*"]
