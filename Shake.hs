#!/usr/bin/env stack
{- stack exec --verbosity info
   --package base-prelude
   --package directory
   --package extra
   --package pandoc
   --package safe
   --package shake
   --package time
   -- ghc -threaded
-}
{-
One of two project scripts files (Makefile, Shake.hs).
This one provides a stronger programming language and more
platform independence than Make. It will build needed packages (above)
on first run and whenever the resolver in stack.yaml changes.
To minimise such startup delays, and reduce sensitivity to git checkout,
compiling is recommended; run the script in interpreted mode to do that.

It requires stack (https://haskell-lang.org/get-started) and
auto-installs the packages above. Also, some rules require:
- site/hakyll-std/hakyll-std
- runhaskell
- groff
- m4
- makeinfo
- git
- patch

Usage: see below. Also:

$ find hledger-lib hledger | entr ./Shake website    # rebuild web files on changes in these dirs

Shake wishes:
just one shake import
wildcards in phony rules
multiple individually accessible wildcards
not having to write :: Action ExitCode after a non-final cmd
-}

{-# LANGUAGE PackageImports, ScopedTypeVariables #-}

import                Prelude ()
import "base-prelude" BasePrelude
import "extra"        Data.List.Extra
import "safe"         Safe
import "shake"        Development.Shake
import "shake"        Development.Shake.FilePath
import "time"         Data.Time
import "directory"    System.Directory as S (getDirectoryContents)

usage = unlines
  ["Usage:"
  ,"./Shake.hs               # compile this script"
  ,"./Shake                  # show commands"
  ,"./Shake docs             # generate built-in manuals (plaintext, man, info)"
  ,"./Shake website          # generate the web site (web manuals, web pages)"
--   ,"./Shake manpages         # generate nroff files for man"
--   ,"./Shake txtmanpages      # generate text man pages for embedding"
--   ,"./Shake infomanpages     # generate info files for info"
--   ,"./Shake webmanpages      # generate individual web man pages for hakyll"
--   ,"./Shake webmanall        # generate all-in-one web manual for hakyll"
--   ,"./Shake cookbookall      # generate all-in-one web cookbook for hakyll"
  ,"./Shake site/doc/VER/.snapshot   # generate and save a versioned web site snapshot"
  ,"./Shake all              # generate everything"
  ,"./Shake clean            # clean generated files"
  ,"./Shake Clean            # clean harder"
  ,"./Shake --help           # show options, eg --color"
  ]

pandoc = "pandoc"                   -- pandoc from PATH (faster)
         --  "stack exec -- pandoc" -- pandoc from project's stackage snapshot
hakyllstd = "site/hakyll-std/hakyll-std"
makeinfo = "makeinfo"
-- nroff = "nroff"
groff = "groff"

main = do

  pandocFilters <-
    map ("tools" </>). nub . sort . map (-<.> "") . filter ("pandoc-" `isPrefixOf`)
    <$> S.getDirectoryContents "tools"

  shakeArgs
    shakeOptions{
      shakeVerbosity=Loud
      -- ,shakeReport=[".shake.html"]
      } $ do

    want ["help"]

    phony "help" $ liftIO $ putStrLn usage

--     phony "compile" $ need ["Shake"]
--
--     "Shake" %> \out -> do
--       need [out <.> "hs"]
--       unit $ cmd "./Shake.hs"  -- running as stack script installs deps and compiles
--       putLoud "You can now run ./Shake instead of ./Shake.hs"

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

      -- manuals rendered to nroff, ready for man (hledger/doc/hledger.1)
      nroffmanpages = [manpageDir m </> m | m <- manpageNames]

      -- manuals rendered to text, ready for embedding (hledger/doc/hledger.1.txt)
      txtmanpages = [manpageDir m </> m <.> "txt" | m <- manpageNames]

      -- manuals rendered to info, ready for info (hledger/doc/hledger.1.info)
      infomanpages = [manpageDir m </> m <.> "info" | m <- manpageNames]

      -- manuals rendered to markdown, ready for web output by hakyll (site/hledger.md)
      webmanpages = ["site" </> manpageNameToUri m <.>"md" | m <- manpageNames]

      -- manuals rendered to markdown and combined, ready for web output by hakyll
      webmanall = "site/manual.md"

      -- cookbook pages in markdown, ready for web output by hakyll (site/csv-import.md).
      -- Keeping these in the main site directory allows hakyll-std to see them (and simpler urls).
      -- These should be ordered like the links on the docs page, so that the combined
      -- cookbook follows the same order.
      cookbookpages = [
         "site/entries.md"
        ,"site/csv-import.md"
        ,"site/account-aliases.md"
        ,"site/account-separator.md"
        ]

      -- cookbook pages combined, ready for web output by hakyll
      cookbookall = "site/cookbook.md"

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
        -- "--filter tools/pandoc-drop-web-blocks"
        "--filter tools/pandoc-drop-html-blocks"
        "--filter tools/pandoc-drop-html-inlines"
        "--filter tools/pandoc-drop-links"
        "--filter tools/pandoc-drop-notes"
        "-o" out

    -- render man page nroffs to fixed-width text for embedding in executables, with nroff
    phony "txtmanpages" $ need txtmanpages

    txtmanpages |%> \out -> do  -- hledger/doc/hledger.1.txt
      let src = dropExtension out
      need [src]
      cmd Shell groff "-t -e -mandoc -Tascii" src  "| col -bx >" out -- http://www.tldp.org/HOWTO/Man-Page/q10.html

    -- use m4 and pandoc to process macros, filter content, and convert to info, suitable for info viewing
    phony "infomanpages" $ need infomanpages

    infomanpages |%> \out -> do -- hledger/doc/hledger.1.info
      let src = out -<.> "m4.md"
          lib = "doc/lib.m4"
          dir = takeDirectory out
      -- assume all other m4 files in dir are included by this one XXX not true in hledger-lib
      deps <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
      need $ src : lib : deps ++ pandocFilters
      cmd Shell
        "m4 -P -I" dir lib src "|"
        pandoc "-f markdown"
        -- "--filter tools/pandoc-drop-web-blocks"
        "--filter tools/pandoc-drop-html-blocks"
        "--filter tools/pandoc-drop-html-inlines"
        "--filter tools/pandoc-drop-links"
        "--filter tools/pandoc-drop-notes"
        "-t texinfo |"
        makeinfo "--force --no-split -o" out

    -- web site

    phony "website" $ do
      need $
        webmanpages ++
        [webmanall
        ,cookbookall
        ,hakyllstd
        ]
      cmd Shell (Cwd "site") "hakyll-std/hakyll-std" "build"
    -- website also links to old manuals, which are generated manually
    -- with ./Shake websnapshot and committed

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
        "--filter tools/pandoc-demote-headers"
        -- "--filter tools/pandoc-add-toc"
        -- "--filter tools/pandoc-drop-man-blocks"
        ">>" out

    -- adjust and combine man page mds for single-page web output, using pandoc
    phony "webmanall" $ need [ webmanall ]

    webmanall %> \out -> do
      need webmanpages
      liftIO $ writeFile webmanall "# Big Manual\n\n* toc\n\n"
      forM_ webmanpages $ \f -> do -- site/hledger.md, site/journal.md
        cmd Shell ("printf '\\n\\n' >>") webmanall :: Action ExitCode
        cmd Shell "pandoc" f "-t markdown --atx-headers"
          -- "--filter tools/pandoc-drop-man-blocks"
          "--filter tools/pandoc-drop-toc"
          -- "--filter tools/pandoc-capitalize-headers"
          "--filter tools/pandoc-demote-headers"
          ">>" webmanall :: Action ExitCode

    -- adjust and combine recipe mds for single-page web output, using pandoc
    phony "cookbookall" $ need [ cookbookall ]

    cookbookall %> \out -> do
      need cookbookpages
      liftIO $ writeFile cookbookall "# User Cookbook\n\n* toc\n\n"
      forM_ cookbookpages $ \f -> do -- site/csv-import.md, site/account-aliases.md, ...
        cmd Shell ("printf '\\n\\n' >>") cookbookall :: Action ExitCode
        cmd Shell "pandoc" f "-t markdown --atx-headers"
          -- "--filter tools/pandoc-drop-man-blocks"
          "--filter tools/pandoc-drop-toc"
          -- "--filter tools/pandoc-capitalize-headers"
          "--filter tools/pandoc-demote-headers"
          ">>" cookbookall :: Action ExitCode

    -- build the currently checked out web docs and save as a named snapshot
    "site/doc/*/.snapshot" %> \out -> do
      need [ webmanall ]
      let snapshot = takeDirectory out
      cmd Shell "mkdir -p" snapshot :: Action ExitCode
      forM_ webmanpages $ \f -> do -- site/hledger.md, site/journal.md
        cmd Shell "cp" f (snapshot </> takeFileName f) :: Action ExitCode
      cmd Shell "cp" "site/manual.md" snapshot :: Action ExitCode
      cmd Shell "cp -r site/images" snapshot :: Action ExitCode
      cmd Shell "touch" out -- :: Action ExitCode

    -- build standard hakyll script used for site rendering
    hakyllstd %> \out -> do
      let dir = takeDirectory out
      need [out <.> "hs", dir </> "TableOfContents.hs"] -- XXX hard-coded dep
      unit $ liftIO $
        cmd (Cwd dir) "./hakyll-std.hs"
        `catch` (\(e::IOException) -> putStr $ unlines $
          ["I could not run ./hakyll-std.hs in "++dir++" to install Hakyll."
          ,"If you see a hakyll-std build error after this, please do it manually:"
          ,"$ (cd site/hakyll-std; ./hakyll-std.hs)"
          ,"and try again."
          ])

    -- cleanup

    phony "clean" $ do
      putNormal "Cleaning generated files"
      removeFilesAfter "." webmanpages
      removeFilesAfter "." [webmanall, cookbookall]
      -- removeFilesAfter "." ["site/doc/[0-9]*"]
      cmd Shell "rm -rf site/doc/[0-9]*"

    phony "Clean" $ do
      need ["clean"]
      putNormal "Cleaning all hakyll generated files"
      removeFilesAfter "site" ["_*"]
      putNormal "Cleaning executables"
      removeFilesAfter "." $ hakyllstd : pandocFilters
      putNormal "Cleaning object files" -- also forces rebuild of executables
      removeFilesAfter "doc"  ["*.o","*.p_o","*.hi"]
      removeFilesAfter "site" ["*.o","*.p_o","*.hi"]
      putNormal "Cleaning shake build files"
      removeFilesAfter ".shake" ["//*"]
