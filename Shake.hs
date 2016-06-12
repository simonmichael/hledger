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
 ./Shake all              # generate everything
 ./Shake docs             # generate general docs
 ./Shake website          # generate the web site
 ./Shake manpages         # generate nroff files for man
 ./Shake txtmanpages      # generate text man pages for embedding
 ./Shake infomanpages     # generate info files for info
 ./Shake webmanpages      # generate web man pages for hakyll
 ./Shake webmanual        # generate combined web man page for hakyll
|]

pandoc = "pandoc"                   -- pandoc from PATH (faster)
         --  "stack exec -- pandoc" -- pandoc from project's stackage snapshot
hakyllstd = "site/hakyll-std/hakyll-std"
makeinfo = "makeinfo"
-- nroff = "nroff"
groff = "groff"

main = do

  pandocFilters <-
    map ("doc" </>). nub . sort . map (-<.> "") . filter ("pandoc-" `isPrefixOf`)
    <$> S.getDirectoryContents "doc"

  shakeArgs
    shakeOptions{
      shakeVerbosity=Loud
      -- ,shakeReport=[".shake.html"]
      } $ do

    want ["help"]

    phony "help" $ liftIO $ putStrLn usage

    phony "compile" $ need ["Shake"]

    "Shake" %> \out -> do
      need [out <.> "hs"]
      cmd "stack ghc Shake.hs" :: Action ExitCode
      putLoud "Compiled ./Shake, you can now use this instead of ./Shake.hs"

    phony "all" $ need ["docs", "website"]

    -- docs

    let
      manpageNames = [ -- in suggested reading order
         "hledger.1"
        ,"hledger-ui.1"
        ,"hledger-web.1"
        ,"hledger-api.1"
        ,"hledger_journal.5"
        ,"hledger_csv.5"
        ,"hledger_timeclock.5"
        ,"hledger_timedot.5"
        ]
      -- manuals m4 source, may include other files (hledger/doc/hledger.1.m4.md)
      m4manpages = [manpageDir m </> m <.> "m4.md" | m <- manpageNames]
      --   manuals rendered to nroff, ready for man (hledger/doc/hledger.1)
      nroffmanpages = [manpageDir m </> m | m <- manpageNames]
      --    manuals rendered to text, ready for embedding (hledger/doc/hledger.1.txt)
      txtmanpages = [manpageDir m </> m <.> "txt" | m <- manpageNames]
      --    manuals rendered to info, ready for info (hledger/doc/hledger.info)
      infomanpages = [manpageDir m </> m <.> "info" | m <- manpageNames]
      --   manuals rendered to markdown, ready for web output by hakyll (site/hledger.md)
      webmanpages = ["site" </> manpageNameToUri m <.>"md" | m <- manpageNames]
      --    manuals rendered to markdown and combined, ready for web output by hakyll
      webmanual = "site/manual.md"

      -- hledger.1 -> hledger/doc, hledger_journal.5 -> hledger-lib/doc
      manpageDir m
        | '_' `elem` m = "hledger-lib" </> "doc"
        | otherwise    = dropExtension m </> "doc"

      -- hledger.1 -> hledger, hledger_journal.5 -> journal
      manpageNameToUri m | "hledger_" `isPrefixOf` m = dropExtension $ drop 8 m
                         | otherwise                 = dropExtension m

      -- hledger -> hledger.1, journal -> hledger_journal.5
      manpageUriToName u | "hledger" `isPrefixOf` u = u <.> "1"
                         | otherwise                = "hledger_" ++ u <.> "5"

    phony "docs" $ do
      need $
        nroffmanpages
        ++ infomanpages
        ++ txtmanpages

    -- compile pandoc helpers
    phony "pandocfilters" $ need pandocFilters

    pandocFilters |%> \out -> do
      need [out <.> "hs"]
      cmd ("stack ghc") out

    -- man pages

    -- use m4 and pandoc to process macros, filter content, and convert to nroff suitable for man output
    phony "manpages" $ need nroffmanpages

    nroffmanpages |%> \out -> do -- hledger/doc/hledger.1
      let src = out <.> "m4.md"
          lib = "doc/lib.m4"
          dir = takeDirectory out
          tmpl = "doc/manpage.nroff"
      -- assume all other m4 files in dir are included by this one XXX not true in hledger-lib
      deps <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
      need $ src : lib : tmpl : deps ++ pandocFilters
      cmd Shell
        "m4 -P -DMAN -I" dir lib src "|"
        pandoc "-f markdown -s --template" tmpl
        -- "--filter doc/pandoc-drop-web-blocks"
        "--filter doc/pandoc-drop-html-blocks"
        "--filter doc/pandoc-drop-html-inlines"
        "--filter doc/pandoc-drop-links"
        "--filter doc/pandoc-drop-notes"
        "-o" out

    -- render man page nroffs to fixed-width text for embedding in executables, with nroff
    phony "txtmanpages" $ need txtmanpages

    txtmanpages |%> \out -> do  -- hledger/doc/hledger.1.txt
      let src = dropExtension out
      need [src]
      cmd Shell groff "-t -e -mandoc -Tascii" src  "| col -bx >" out -- http://www.tldp.org/HOWTO/Man-Page/q10.html

    -- use m4 and pandoc to process macros, filter content, and convert to info, suitable for info viewing
    phony "infomanpages" $ need infomanpages

    infomanpages |%> \out -> do -- hledger/doc/hledger.info
      let src = out -<.> "m4.md"
          lib = "doc/lib.m4"
          dir = takeDirectory out
      -- assume all other m4 files in dir are included by this one XXX not true in hledger-lib
      deps <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
      need $ src : lib : deps ++ pandocFilters
      cmd Shell
        "m4 -P -I" dir lib src "|"
        pandoc "-f markdown"
        -- "--filter doc/pandoc-drop-web-blocks"
        "--filter doc/pandoc-drop-html-blocks"
        "--filter doc/pandoc-drop-html-inlines"
        "--filter doc/pandoc-drop-links"
        "--filter doc/pandoc-drop-notes"
        "-t texinfo |"
        makeinfo "--force --no-split -o" out

    -- web site

    phony "website" $ do
      need $ 
        webmanpages ++
        [webmanual
        ,"releasemanual"
        ,hakyllstd
        ]
      cmd Shell (Cwd "site") "hakyll-std/hakyll-std" "build"

    -- use m4 and pandoc to process macros and filter content, leaving markdown suitable for web output
    phony "webmanpages" $ need webmanpages

    webmanpages |%> \out -> do -- site/hledger.md
      let m       = manpageUriToName $ dropExtension $ takeFileName out -- hledger.1
          dir     = manpageDir m
          src     = dir </> m <.> "m4.md"
          lib     = "doc/lib.m4"
          heading = let h = dropExtension m
                    in if "hledger_" `isPrefixOf` h
                       then drop 8 h ++ " format"
                       else h
      -- assume all other m4 files in dir are included by this one XXX not true in hledger-lib
      deps <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
      need $ src : lib : deps ++ pandocFilters
      liftIO $ writeFile out $ "# " ++ heading ++ "\n\n"
      cmd Shell
        "m4 -P -DMAN -DWEB -I" dir lib src "|"
        pandoc "-f markdown -t markdown --atx-headers"
        "--filter doc/pandoc-demote-headers"
        -- "--filter doc/pandoc-add-toc"
        -- "--filter doc/pandoc-drop-man-blocks"
        ">>" out

    -- adjust and combine man page mds for single-page web output, using pandoc
    phony "webmanual" $ need [ webmanual ]

    webmanual %> \out -> do 
      need webmanpages
      liftIO $ writeFile webmanual "* toc\n\n"
      forM_ webmanpages $ \f -> do -- site/hledger.md, site/journal.md
        cmd Shell ("printf '\\n\\n' >>") webmanual :: Action ExitCode
        cmd Shell "pandoc" f "-t markdown --atx-headers"
          -- "--filter doc/pandoc-drop-man-blocks"
          "--filter doc/pandoc-drop-toc"
          -- "--filter doc/pandoc-capitalize-headers"
          "--filter doc/pandoc-demote-headers"
          ">>" webmanual :: Action ExitCode

    -- check out and render manual pages for the current release also
    phony "releasemanual" $ need [ "releasemanual0.27" ]

    phony "releasemanual0.27" $ do
      -- XXX under doc so hakyll-std will render it
      cmd "mkdir -p site/doc/0.27" :: Action ExitCode
      cmd Shell "git show 0.27:doc/manual.md >site/doc/0.27/manual.md"

    -- build standard hakyll script used for site rendering
    hakyllstd %> \out -> do
      let dir = takeDirectory out
      need [out <.> "hs", dir </> "TableOfContents.hs"] -- XXX hard-coded dep
      cmd (Cwd dir) "stack ghc hakyll-std"

    -- cleanup

    phony "clean" $ do
      putNormal "Cleaning generated files"
      removeFilesAfter "." webmanpages
      removeFilesAfter "." [webmanual]

    phony "Clean" $ do
      need ["clean"]
      putNormal "Cleaning generated man page nroffs"
      removeFilesAfter "." nroffmanpages
      putNormal "Cleaning all hakyll generated files"
      removeFilesAfter "site" ["_*"]
      putNormal "Cleaning executables"
      removeFilesAfter "." $ hakyllstd : pandocFilters
      putNormal "Cleaning object files"
      removeFilesAfter "doc"  ["*.o","*.p_o","*.hi"] -- forces rebuild of exes ?
      removeFilesAfter "site" ["*.o","*.p_o","*.hi"]
      putNormal "Cleaning shake build files"
      removeFilesAfter ".shake" ["//*"]
