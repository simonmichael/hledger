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
 ./Shake txtmanpages      # generate text man pages for embedding
 ./Shake webmanpages      # generate web man pages for hakyll
 ./Shake webmanual        # generate combined web man page for hakyll
|]

pandoc =
  -- "stack exec -- pandoc" -- use the pandoc required above
  "pandoc"                  -- use pandoc in PATH (faster)
hakyllstd = "site/hakyll-std/hakyll-std"
nroff = "nroff"

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
      need ["Shake.hs"]
      cmd "stack ghc Shake.hs" :: Action ExitCode
      putLoud "Compiled ./Shake, you can now use this instead of ./Shake.hs"

    -- docs

    phony "docs" $ do
      need [
          "manpages"
         ,"txtmanpages"
         ]

    let webmanual = "site/manual.md"

    phony "site" $ do
      need [
         "webmanpages"
        ,webmanual
        ,hakyllstd
        ]
      cmd Shell (Cwd "site") "hakyll-std/hakyll-std" "build"

    hakyllstd %> \out -> do
      let dir = takeDirectory out
      need [out <.> "hs", dir </> "TableOfContents.hs"]
      cmd (Cwd dir) "stack ghc hakyll-std"

    -- man pages

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

    -- some man pages have their md source assembled from parts with m4
    let m4manpages = [manpageDir m </> m <.> ".md" | m <- ["hledger.1"]] -- hledger/doc/hledger.1.md
    m4manpages |%> \out -> do     -- hledger/doc/hledger.1.md
      let dir = takeDirectory out -- hledger/doc
          m4src = out -<.> "m4" <.> "md"    -- hledger/doc/hledger.1.m4.md
          m4lib = "doc/lib.m4"
      -- assume all other m4 files in dir are included by this one
      m4deps <- liftIO $ filter (/= m4src) . filter (".m4.md" `isSuffixOf`) . map (dir </>)
                <$> S.getDirectoryContents dir
      need $ m4src : m4lib : m4deps
      cmd Shell "m4 -P -DWEB -DMAN -I" dir m4lib m4src ">" out

    -- compile pandoc filters, used eg for adjusting manpage md source for web or man output
    phony "pandocfilters" $ need pandocFilters
    pandocFilters |%> \out -> do
      need [out <.> "hs"]
      cmd ("stack ghc") out

    -- adjust man page mds for man output and convert to nroff, with pandoc
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

    -- render man page nroffs as fixed-width text, for embedding
    let txtmanpages = [m <.> "txt" | m <- manpages] -- hledger/doc/hledger.1.txt, hledger-lib/doc/journal.5.txt
    phony "txtmanpages" $ need txtmanpages
    txtmanpages |%> \out -> do
      let nroffsrc = dropExtension out  -- hledger/doc/hledger.1
      cmd Shell nroff "-man" nroffsrc ">" out

    -- adjust man page mds for (hakyll) web output, with pandoc
    let webmanpages = ["site" </> manpageNameToUri m <.>".md" | m <- manpageNames] -- site/hledger.md, site/journal.md
    phony "webmanpages" $ need webmanpages
    webmanpages |%> \out -> do
      let m = manpageUriToName $ dropExtension $ takeFileName out  -- hledger.1
          md = manpageDir m </> m <.> "md"                         -- hledger/doc/hledger.1.md
          heading = let h = dropExtension m
                    in if "hledger_" `isPrefixOf` h
                       then drop 8 h ++ " format"
                       else h
      need $ md : pandocFilters
      liftIO $ writeFile out $ "# " ++ heading ++ "\n\n"
      cmd Shell pandoc md "-t markdown --atx-headers"
        "--filter doc/pandoc-demote-headers"
        -- "--filter doc/pandoc-add-toc"
        -- "--filter doc/pandoc-drop-man-blocks"
        ">>" out

    -- adjust and combine man page mds for single-page web output, using pandoc

    phony "webmanual" $ need [ webmanual ]
    webmanual %> \out -> do
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
        -- let heading =
        --       let h = dropExtension $ takeFileName f -- hledger, journal
        --       in if "hledger" `isPrefixOf` h
        --          then h                              -- hledger
        --          else h ++ " format"                 -- journal format
        -- cmd Shell ("printf '\\n## "++ heading ++"\\n\\n' >>") webmanual :: Action ExitCode
        cmd Shell ("printf '\\n\\n' >>") webmanual :: Action ExitCode
        cmd Shell "pandoc" f "-t markdown --atx-headers"
          -- "--filter doc/pandoc-drop-man-blocks"
          "--filter doc/pandoc-drop-toc"
          -- "--filter doc/pandoc-capitalize-headers"
          "--filter doc/pandoc-demote-headers"
          ">>" webmanual :: Action ExitCode

    -- cleanup

    phony "clean" $ do
      putNormal "Cleaning generated files"
      removeFilesAfter "." ["hledger/doc/hledger.1.md"]
      removeFilesAfter "." webmanpages
      removeFilesAfter "." [webmanual]

    phony "Clean" $ do
      need ["clean"]
      putNormal "Cleaning generated man page nroffs"
      removeFilesAfter "." manpages
      putNormal "Cleaning all hakyll generated files"
      removeFilesAfter "site" ["_*"]
      putNormal "Cleaning executables"
      removeFilesAfter "." $ hakyllstd : pandocFilters
      putNormal "Cleaning object files"
      removeFilesAfter "doc"  ["*.o","*.p_o","*.hi"] -- forces rebuild of exes ?
      removeFilesAfter "site" ["*.o","*.p_o","*.hi"]
      putNormal "Cleaning shake build files"
      removeFilesAfter ".shake" ["//*"]
