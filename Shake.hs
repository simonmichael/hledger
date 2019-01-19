#!/usr/bin/env stack
{- stack exec
   --verbosity=info
   --package base-prelude
   --package directory
   --package extra
   --package safe
   --package shake
   --package time
   ghc
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
- runhaskell
- groff
- m4
- makeinfo
- git
- patch

Usage: see below. Also:

$ find hledger-lib hledger | entr ./Shake website    # rebuild web files on changes in these dirs

Shake rule dependency graph:
file:///Users/simon/src/PLAINTEXTACCOUNTING/hledger/report.html?mode=rule-graph&query=!name(/(doc%7Cimages%7Cjs%7Ccss%7Cfonts%7Ctime%7Capi%7Cui%7Ccsv)/)

Shake wishes:
just one shake import
wildcards in phony rules
multiple individually accessible wildcards
not having to write :: Action ExitCode after a non-final cmd
-}

{-# LANGUAGE PackageImports, ScopedTypeVariables #-}

import                Prelude ()
import "base-prelude" BasePrelude
import "directory"    System.Directory as S (getDirectoryContents)
import "extra"        Data.List.Extra
import "safe"         Safe
import "shake"        Development.Shake
import "shake"        Development.Shake.FilePath
import "time"         Data.Time
-- import "hledger-lib"  Hledger.Utils.Debug

usage = unlines
  ["Usage:"
  ,"./Shake.hs               # compile this script"
  ,"./Shake manuals          # generate the txt/man/info manuals"
  ,"./Shake website          # generate the website and web manuals"
  ,"./Shake all              # generate everything"
  ,""
  ,"./Shake site/doc/VERSION/.snapshot   # save the checked-out web manuals as a versioned snapshot"
  ,"./Shake clean            # clean generated files"
  ,"./Shake Clean            # clean more thoroughly"
  ,""
  ,"./Shake [help]           # show commands"
  ,"./Shake --help           # show detailed Shake options, eg --color"
  ]

groff    = "groff"
makeinfo = "makeinfo"
pandoc   = "pandoc"

-- The kind of markdown used in our doc source files.
fromsrcmd = "-f markdown-tex_math_dollars"

-- The kind of markdown we like to generate for the website.
towebmd = "-t markdown-fenced_divs --atx-headers"


main = do

  shakeArgs
    shakeOptions{
      shakeVerbosity=Loud
      -- ,shakeReport=[".shake.html"]
      } $ do

    want ["help"]

    phony "help" $ liftIO $ putStrLn usage

    phony "all" $ need ["manuals", "website"]

    -- phony "compile" $ need ["Shake"]
    -- "Shake" %> \out -> do
    --   need [out <.> "hs"]
    --   unit $ cmd "./Shake.hs"  -- running as stack script installs deps and compiles
    --   putLoud "You can now run ./Shake instead of ./Shake.hs"


    -- MANUALS

    let
      -- documentation versions shown on the website (excluding 0.27 which is handled specially)
      docversions = [ "1.0" , "1.1" , "1.2" , "1.3" , "1.4" , "1.5" , "1.9", "1.10", "1.11", "1.12" ]

      -- names, files, uris:

      -- man page names (manual names plus a man section number), in suggested reading order
      manpageNames = [
         "hledger.1"
        ,"hledger-ui.1"
        ,"hledger-web.1"
        ,"hledger-api.1"
        ,"hledger_journal.5"
        ,"hledger_csv.5"
        ,"hledger_timeclock.5"
        ,"hledger_timedot.5"
        ]

      -- basic manual names, without numbers
      manualNames = map manpageNameToManualName manpageNames

      -- main markdown+m4 source files for manuals (hledger/hledger.m4.md)
      -- These may include additional files using m4.
      m4manuals = [manualDir m </> m <.> "m4.md" | m <- manualNames]

      -- manuals rendered to nroff, ready for man (hledger/hledger.1)
      nroffmanuals = [manpageDir m </> m | m <- manpageNames]

      -- manuals rendered to plain text, ready for embedding (hledger/hledger.txt)
      txtmanuals = [manualDir m </> m <.> "txt" | m <- manualNames]

      -- manuals rendered to info, ready for info (hledger/hledger.info)
      infomanuals = [manualDir m </> m <.> "info" | m <- manualNames]

      -- manuals rendered to markdown, ready for conversion to html (site/hledger.md)
      webmanuals = ["site" </> manpageNameToUri m <.> "md" | m <- manpageNames]

      -- website html pages - all manual versions plus misc pages in site/ or copied from elsewhere.
      -- All these names will have lower-case URIs on the website.
      webhtmlpages
        = map (normalise . ("site/_site" </>))
            $ ( [ prefix </> manpageNameToUri mPage <.> "html"
                   | prefix <- "" : [ "doc" </> v | v <- docversions ]
                   , mPage  <- manpageNames
                ]
             ++ [ mPage <.> "html"
                   | mPage <- [
                         "contributors"
                       , "download"
                       , "ledgertips"
                       , "index"
                       , "intro"
                       , "release-notes"
                       ]
                ]
             ++ [ prefix </> "manual" <.> "html"
                   | prefix <- "" : "doc/0.27" : [ "doc" </> v | v <- docversions ]
                ]
              )

      -- manuals rendered to markdown and combined, ready for web rendering
      webmancombined = "site/manual.md"

      -- extensions of static web asset files, to be copied to the website
      webassetexts = ["png", "gif", "cur", "js", "css", "eot", "ttf", "woff", "svg"]

      -- The directory in which to find this man page.
      -- hledger.1 -> hledger/doc, hledger_journal.5 -> hledger-lib/doc
      manpageDir m
        | '_' `elem` m = "hledger-lib"
        | otherwise    = dropExtension m

      -- The directory in which to find this manual.
      -- hledger -> hledger, hledger_journal -> hledger-lib
      manualDir m
        | '_' `elem` m = "hledger-lib"
        | otherwise    = m

      -- The URI corresponding to this man page.
      -- hledger.1 -> hledger, hledger_journal.5 -> journal
      manpageNameToUri m | "hledger_" `isPrefixOf` m = dropExtension $ drop 8 m
                         | otherwise                 = dropExtension m

      -- The man page corresponding to this URI.
      -- hledger -> hledger.1, journal -> hledger_journal.5
      manpageUriToName u | "hledger" `isPrefixOf` u = u <.> "1"
                         | otherwise                = "hledger_" ++ u <.> "5"

    -- Generate the manuals in nroff, plain text and info formats.
    phony "manuals" $ do
      need $
        nroffmanuals
        ++ infomanuals
        ++ txtmanuals

    -- Generate nroff man pages suitable for man output.
    phony "manmanuals" $ need nroffmanuals
    nroffmanuals |%> \out -> do -- hledger/hledger.1
      let src = manpageNameToManualName out <.> "m4.md"
          lib = "doc/lib.m4"
          dir = takeDirectory out
          tmpl = "doc/manpage.nroff"
      -- assume all other m4 files in dir are included by this one XXX not true in hledger-lib
      deps <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
      need $ src : lib : tmpl : deps
      cmd Shell
        "m4 -P -DMAN -I" dir lib src "|"
        pandoc fromsrcmd "-s" "--template" tmpl
        "--lua-filter tools/pandoc-drop-html-blocks.lua"
        "--lua-filter tools/pandoc-drop-html-inlines.lua"
        "--lua-filter tools/pandoc-drop-links.lua"
        "-o" out

    -- Generate plain text manuals suitable for embedding in
    -- executables and viewing with a pager.
    phony "txtmanuals" $ need txtmanuals
    txtmanuals |%> \out -> do  -- hledger/hledger.txt
      let src = manualNameToManpageName $ dropExtension out
      need [src]
      cmd Shell groff "-t -e -mandoc -Tascii" src  "| col -bx >" out -- http://www.tldp.org/HOWTO/Man-Page/q10.html

    -- Generate Info manuals suitable for viewing with info.
    phony "infomanuals" $ need infomanuals
    infomanuals |%> \out -> do -- hledger/hledger.info
      let src = out -<.> "m4.md"
          lib = "doc/lib.m4"
          dir = takeDirectory out
      -- assume all other m4 files in dir are included by this one XXX not true in hledger-lib
      deps <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
      need $ src : lib : deps
      cmd Shell
        "m4 -P -I" dir lib src "|"
        pandoc fromsrcmd
        "--lua-filter tools/pandoc-drop-html-blocks.lua"
        "--lua-filter tools/pandoc-drop-html-inlines.lua"
        "--lua-filter tools/pandoc-drop-links.lua"
        "-t texinfo |"
        makeinfo "--force --no-split -o" out


    -- WEBSITE MARKDOWN SOURCE

    -- Generate the individual web manuals' markdown source, using m4
    -- and pandoc to tweak content.
    phony "webmanuals" $ need webmanuals
    webmanuals |%> \out -> do -- site/hledger.md
      let manpage = manpageUriToName $ dropExtension $ takeFileName out -- hledger
          manual  = manpageNameToManualName manpage
          dir     = manpageDir manpage
          src     = dir </> manual <.> "m4.md"
          lib     = "doc/lib.m4"
          heading = let h = manual
                    in if "hledger_" `isPrefixOf` h
                       then drop 8 h ++ " format"
                       else h
      -- assume all other m4 files in dir are included by this one XXX not true in hledger-lib
      deps <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
      need $ src : lib : deps
      liftIO $ writeFile out $ "# " ++ heading ++ "\n\n"
      cmd Shell
        "m4 -P -DMAN -DWEB -I" dir lib src "|"
        pandoc fromsrcmd towebmd
        "--lua-filter tools/pandoc-demote-headers.lua"
        ">>" out

    -- Generate the combined web manual's markdown source, by
    -- concatenating tweaked versions of the individual manuals.
    phony "webmancombined" $ need [ webmancombined ]
    webmancombined %> \out -> do
      need webmanuals
      liftIO $ writeFile webmancombined "\\$toc\\$" -- # Big Manual\n\n -- TOC style is better without main heading,
      forM_ webmanuals $ \f -> do -- site/hledger.md, site/journal.md
        cmd Shell ("printf '\\n\\n' >>") webmancombined :: Action ExitCode
        cmd Shell pandoc f towebmd
          "--lua-filter tools/pandoc-drop-toc.lua"
          "--lua-filter tools/pandoc-demote-headers.lua"
          ">>" webmancombined :: Action ExitCode


    -- WEBSITE HTML & ASSETS

    phony "website" $ need $ [ "webassets" , "webhtml" ]

    -- copy all static asset files (files with certain extensions
    -- found under sites, plus one or two more) to sites/_site/
    phony "webassets" $ do
        assets <- getDirectoryFiles "site" (map ("//*" <.>) webassetexts)
        need [ "site/_site" </> file
                | file <- assets ++ [
                    "files/README"
                    ]
                , not ("_site//*" ?== file)
             ]

    -- copy any one of the static asset files to sites/_site/
    "site/_site/files/README" : [ "site/_site//*" <.> ext | ext <- webassetexts ] |%> \out -> do
        copyFile' ("site" </> dropDirectory2 out) out

    -- render all website pages as html, saved in sites/_site/
    phony "webhtml" $ need webhtmlpages

    -- render one website page as html, saved in sites/_site/
    "site/_site//*.html" %> \out -> do
        let source    = "site" </> dropDirectory2 out -<.> "md"
            pageTitle = takeBaseName out
            template  = "site/site.tmpl"
            siteRoot  = if "site/_site/doc//*" ?== out then "../.." else "."
        need [source, template]
        cmd Shell pandoc fromsrcmd "-t html" source
                         "--template"                template
                         ("--metadata=siteRoot:"  ++ siteRoot)
                         ("--metadata=title:"     ++ pageTitle)
                         "--lua-filter"              "tools/pandoc-site.lua"
                         "--output"                  out


    -- MISC

    -- Generate the web manuals based on the current checkout and save
    -- them as the specified versioned snapshot in site/doc/VER/ .
    -- .snapshot is a dummy file.
    "site/doc/*/.snapshot" %> \out -> do
      need $ webmancombined : webmanuals
      let snapshot = takeDirectory out
      cmd Shell "mkdir -p" snapshot :: Action ExitCode
      forM_ webmanuals $ \f -> do -- site/hledger.md, site/journal.md
        cmd Shell "cp" f (snapshot </> takeFileName f) :: Action ExitCode
      cmd Shell "cp" "site/manual.md" snapshot :: Action ExitCode
      cmd Shell "cp -r site/images" snapshot :: Action ExitCode
      cmd Shell "touch" out -- :: Action ExitCode

    phony "clean" $ do
      putNormal "Cleaning generated files"
      removeFilesAfter "." webmanuals
      removeFilesAfter "." [webmancombined]

    phony "Clean" $ do
      need ["clean"]
      putNormal "Cleaning all site generated files"
      removeFilesAfter "site" ["_*"]
      putNormal "Cleaning object files" -- also forces rebuild of executables
      removeFilesAfter "tools"  ["*.o","*.p_o","*.hi"]
      removeFilesAfter "site" ["*.o","*.p_o","*.hi"]
      putNormal "Cleaning shake build files"
      removeFilesAfter ".shake" ["//*"]


-- Convert numbered man page names to manual names.
-- hledger.1 -> hledger, hledger_journal.5 -> hledger_journal
manpageNameToManualName = dropNumericSuffix
  where
    dropNumericSuffix s = reverse $
      case reverse s of
        c : '.' : cs | isDigit c -> cs
        cs                       -> cs

-- Convert manual names to numbered man page names.
-- hledger -> hledger.1, hledger_journal -> hledger_journal.5
manualNameToManpageName s
  | '_' `elem` s = s <.> "5"
  | otherwise    = s <.> "1"

dropDirectory2 = dropDirectory1 . dropDirectory1

