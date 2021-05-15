#!/usr/bin/env stack
{- stack script --resolver lts-17.4 --compile
   --package base-prelude
   --package directory
   --package extra
   --package process
   --package regex
   --package safe
   --package shake
   --package time
-}
{-
-- add this to see packages being installed instead of a long silence:
   --verbosity=info

This is one of two collections of maintainer/developer scripts; Makefile is the other.
This one, based on shake, provides a stronger programming language and
more platform independence. It requires stack and will auto-install
the haskell packages above when needed.

Some of the commands below require additional command-line tools, including:
- GNU date (on mac: brew install coreutils)
- groff
- m4
- makeinfo
- pandoc
- sed

Some things that may be useful when working on this:
- https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter
- watch Shake.hs for compile errors: make ghcid-shake
- load Shake.hs in GHCI: make ghci-shake
- rebuild things when files change with entr (file watcher), eg:
   find hledger-lib hledger | entr ./Shake manuals
- view rule dependency graph:
   ./Shake --report, open report.html?mode=rule-graph&query=!name(/(doc%7Cimages%7Cjs%7Ccss%7Cfonts%7Ctime%7Capi%7Cui%7Ccsv)/)

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import                Prelude ()
import "base-prelude" BasePrelude
import "base"         Control.Exception as C
-- required packages, keep synced with Makefile -> SHAKEDEPS:
import "directory"    System.Directory as S (getDirectoryContents)
import "extra"        Data.List.Extra hiding (headDef, lastDef)
import "process"      System.Process
import "regex"        Text.RE.TDFA.String
import "regex"        Text.RE.Replace
import "safe"         Safe
import "shake"        Development.Shake
import "shake"        Development.Shake.FilePath
import "time"         Data.Time
-- import "hledger-lib"  Hledger.Utils.Debug

usage =
  let scriptname = "Shake" in replaceRe [re|/Shake|] ('/':scriptname) $
  unlines
    ---------------------------------------79--------------------------------------
  ["hledger developer scripts. See also: make help"
  ,"Usage:"
  ,"./Shake.hs [CMD [ARGS]]  run CMD, compiling this script first if needed"
  ,"./Shake    [CMD [ARGS]]  run CMD, using the compiled version of this script"
  ,"./Shake [help]           show this help"
  ,"./Shake cabalfiles [-c]  update */*.cabal files from */package.yaml"
  ,"./Shake setversion [VER] [PKGS] [-c]"
  ,"                         update versions in source files to */.version or VER"
  ,"                         and update */*.cabal files"
  ,"./Shake cmdhelp [-c]     update hledger CLI commands' help texts"
  ,"./Shake mandates         update the date shown in some manual formats"
  ,"./Shake manuals [-c]     update all packages' txt/man/info/web manuals"
  -- ,"./Shake webmanuals       update just the web manuals"
  ,"./Shake changelogs [-c] [-n/--dry-run]"
  ,"                         update CHANGES.md files, adding new commits & headings"
  ,"./Shake docs [-c]        update all program docs (CLI help, manuals, changelogs)"
  ,"./Shake build [PKGS]     build hledger packages and their embedded docs"
  ,"./Shake clean            remove generated texts, manuals"
  ,"./Shake Clean            also remove object files, Shake's cache"
  ,"./Shake FILE             build any individual file"
  ,"./Shake --help           list shake build options (--color, --rebuild, etc."
  ,"                         Keep shake option arguments adjacent to their flag.)"
  ,""
  ,"See comments in Shake.hs for more detailed descriptions."
  ,"Add -c/--commit to have commands commit their changes."
  ,"Add -V/-VV/-VVV to see more verbose output."
  ,"Add -B, with nothing immediately after it, to force rebuilding."
  ]
-- TODO
--  ,"./Shake releasebranch      create a new release branch, bump master to next dev version (.99)" 
--  ,"./Shake majorversion       bump to the next major version project-wide, update affected files"
--  ,"./Shake minorversion PKGS  bump one or more packages to their next minor version project-wide, update affected files"
--  ,"./Shake relnotes           create draft release notes"

-- groff    = "groff -c" ++ " -Wall"  -- see "groff" below
m4 = "m4 -P"
makeinfo = "makeinfo --no-split --force --no-warn --no-validate"  -- silence makeinfo warnings, comment these to see them
pandoc   = "pandoc --strip-comments"
gitcommit = "git commit --allow-empty"

-- We should work with both BSD and GNU sed. Tips:
-- use [a-z] [0-9] instead of \w \d etc.
-- backslash-escape things like: { &
sed = "sed -E"

-- We should work with both BSD and GNU grep.
grep = "grep -E"

-- The kind of markdown used in our doc source files.
fromsrcmd = "-f markdown-smart-tex_math_dollars"

-- The kind of markdown we like to generate for the website.
-- This will be consumed by sphinx extensions:
--  recommonmark (Commonmark syntax, https://spec.commonmark.org)
--  sphinx-markdown-tables (PHP Markdown Extra table syntax, https://michelf.ca/projects/php-markdown/extra/#table)
-- XXX trying to force the use of pipe_tables here, but sometimes it uses html instead
-- --markdown-headings=atx requires pandoc 2.11.2+, with older pandoc use --atx-headers instead
towebmd = "-t markdown-smart-fenced_divs-fenced_code_attributes-simple_tables-multiline_tables-grid_tables-raw_attribute --markdown-headings=atx"


main = do

  -- Gather some IO values used by rules.

  -- hledger manual also includes the markdown files from here:
  let commandsdir = "hledger/Hledger/Cli/Commands"
  commandmds <-
    filter (not . ("README." `isPrefixOf`) . takeFileName) . filter (".md" `isSuffixOf`) . map (commandsdir </>)
    <$> S.getDirectoryContents commandsdir
  let commandtxts = map (-<.> "txt") commandmds

  let sitedir = "site"
  pages <- map takeBaseName . filter (".md" `isSuffixOf`) <$> S.getDirectoryContents sitedir

  -- Run the shake rule selected by the first command line argument.
  -- Other arguments and some custom flags are set aside for the rule
  -- to use if it wants.

  -- Option arguments should be kept adjacent to their flag or this will go wrong.
  (opts, args) <- partition ("-" `isPrefixOf`) <$> getArgs
  let
    ruleoptnames = [
       "--commit", "-c"
      ,"--dry-run", "--dry", "-n"
      ]
    (ruleopts, shakeopts) = partition (`elem` ruleoptnames) opts
    commit = any (`elem` ruleopts) ["--commit", "-c"]
    dryrun = any (`elem` ruleopts) ["--dry-run", "--dry", "-n"]
    (shakearg, ruleargs) = splitAt 1 args
    shakeargs = shakeopts ++ shakearg
  -- print (opts,args,shakeopts,shakearg,shakeargs,ruleopts,ruleargs)

  withArgs shakeargs $ shakeArgs shakeOptions{
    shakeVerbosity=Quiet
    -- ,shakeReport=[".shake.html"]
    }
    $ do
   
  -- The rules.
  
      want ["help"]

      phony "help" $ liftIO $ putStr usage

      -- NAMES, FILES, URIS, HELPERS

      let
        -- main package names, in standard build order
        packages = [
           "hledger-lib"
          ,"hledger"
          ,"hledger-ui"
          ,"hledger-web"
          ]
        pkgdirs = packages
        pkgandprojdirs = "" : pkgdirs
        cabalfiles = [p </> p <.> "cabal" | p <- packages]
        changelogs = map (</> "CHANGES.md") pkgandprojdirs
        packagemanversionm4s = [p </> ".version.m4" | p <- packages]
        packagemandatem4s = [p </> ".date.m4" | p <- packages]

        -- doc files (or related targets) that should be generated
        -- before building hledger packages.
        -- [(PKG, [TARGETS])]
        embeddedFiles = [
           -- hledger embeds the plain text command help files and all packages' text/nroff/info manuals
           ("hledger", commandtxts ++ ["manuals"])
           -- hledger-ui imports the hledger-ui manuals from hledger
          ,("hledger-ui", ["hledger"])
          ]

        -- man page names (manual names plus a man section number), in suggested reading order
        manpageNames = [
           "hledger.1"
          ,"hledger-ui.1"
          ,"hledger-web.1"
          ]

        -- basic manual names, without numbers
        manualNames = map manpageNameToManualName manpageNames

        -- main markdown+m4 source files for manuals (hledger/hledger.m4.md)
        -- These may include additional files using m4.
        m4manuals = [manualDir m </> m <.> "m4.md" | m <- manualNames]

        -- manuals as plain text, ready for embedding as CLI help (hledger/hledger.txt)
        txtmanuals = [manualDir m </> m <.> "txt" | m <- manualNames]

        -- manuals as nroff, ready for man (hledger/hledger.1)
        nroffmanuals = [manpageDir m </> m | m <- manpageNames]

        -- manuals as info, ready for info (hledger/hledger.info)
        infomanuals = [manualDir m </> m <.> "info" | m <- manualNames]

        -- manuals as sphinx-ready markdown, to be rendered as part of the website (hledger/hledger.md)
        webmanuals = [manualDir m </> m <.> "md" | m <- manualNames]

        -- -- old versions of the manuals rendered to html (site/_site/doc/1.14/hledger.html)
        -- oldhtmlmanuals = map (normalise . ("site/_site/doc" </>) . (<.> "html")) $
        --   [ v </> manpageNameToWebManualName p | v <- docversions, v>="1.0", p <- manpageNames ++ ["manual"] ] ++
        --   [ v </> "manual"           | v <- docversions, v <"1.0" ]  -- before 1.0 there was only the combined manual

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

        -- The web manual name (& URI "slug") corresponding to this man page.
        -- hledger.1 -> hledger, hledger_journal.5 -> journal
        manpageNameToWebManualName m | "hledger_" `isPrefixOf` m = dropExtension $ drop 8 m
                                     | otherwise                 = dropExtension m

        -- The man page corresponding to this web manual name.
        -- hledger -> hledger.1, journal -> hledger_journal.5
        webManualNameToManpageName u | "hledger" `isPrefixOf` u = u <.> "1"
                                     | otherwise                = "hledger_" ++ u <.> "5"

      -- VERSION NUMBERS

      -- Regenerate .cabal files from package.yaml files, using stack build --dry-run.
      -- (used by "cabalfiles" and "setversion")
      let docabalfiles = do
          -- stack can fail to update cabal files with zero exit status,
          -- so we need to to check stderr, and specifically for the error message 
          -- since all output goes there
          err <- fromStderr <$>
            -- stack 1.7 no longer updates cabal files with --dry-run, must do a full build
            -- (or use hpack, of similar version)
            (cmd Shell "stack build" :: Action (Stderr String))
          when ("was generated with a newer version of hpack" `isInfixOf` err) $
            liftIO $ putStr err >> exitFailure
          when commit $ do
            let msg = ";update cabal files"
            cmd Shell gitcommit ("-m '"++msg++"' --") cabalfiles

      -- Update version strings in most "source" files to match what's in PKG/.version.
      -- If a version number is provided as first argument, save that in PKG/.version files first.
      -- If one or more subdirectories are provided as arguments, save/update only those.
      -- Also regenerates .cabal files from package.yaml files.
      -- See also CONTRIBUTING.md > Version numbers.
      phony "setversion" $ do
        let
          (mver, dirargs) = (headMay ver', drop 1 ver' ++ dirs')
            where (ver',dirs') = span isVersion ruleargs
          (specifieddirs, specifiedpkgs) =
            case dirargs of [] -> (pkgandprojdirs, pkgdirs)
                            ds -> (ds, ds)
        -- if a version was provided, update .version files in the specified directories
        let specifiedversionfiles = map (</> ".version") specifieddirs
        case mver of
          Just v  -> liftIO $ forM_ specifiedversionfiles $ flip maybeWriteFile (v++"\n")
          Nothing -> return ()

        -- update "source" files depending on .version in the specified packages
        let dependents = concat [
               map (</> ".version.m4")  specifiedpkgs
              ,map (</> "package.yaml") specifiedpkgs
              ]
        need dependents

        -- and maybe commit them
        when commit $ do
          let msg = unwords [
                 ";bump"
                ,case dirargs of
                   [] -> "version"
                   ds -> intercalate ", " ds ++ " version"
                ,case mver of
                   Nothing -> ""
                   Just v  -> "to " ++ v
                ]
          cmd Shell gitcommit ("-m '"++msg++"' --") specifiedversionfiles dependents

        docabalfiles

      -- PKG/.version.m4 <- PKG/.version, just updates the _version_ macro
      "hledger*/.version.m4" %> \out -> do
        let versionfile = takeDirectory out </> ".version"
        need [versionfile]
        version <- ((head . words) <$>) $ liftIO $ readFile versionfile
        cmd_ Shell sed "-i -e" ("'s/(_version_}}, *)\\{\\{[^}]+/\\1{{"++version++"/;'") out

      -- PKG/package.yaml <- PKG/.version, just updates version strings
      "hledger*/package.yaml" %> \out -> do
        let versionfile = takeDirectory out </> ".version"
        need [versionfile]
        version <- ((head . words) <$>) $ liftIO $ readFile versionfile
        let ma:jor:_ = splitOn "." version
            nextmajorversion = intercalate "." $ ma : (show $ read jor+1) : []

        -- One simple task: update some strings in a small text file.
        -- Several ugly solutions:
        --
        -- 1. use haskell list utils. Tedious.
        -- old <- liftIO $ readFileStrictly out
        -- let isversionline s = "version" `isPrefixOf` (dropWhile isSpace $ takeWhile (not.(`elem` " :")) s)
        --     (before, _:after) = break isversionline $ lines old
        --     -- oldversion = words versionline !! 1
        --     new = unlines $ before ++ ["version: "++version] ++ after
        -- liftIO $ writeFile out new
        --
        -- 2. use regular expressions in haskell. Haskell has no portable,
        -- featureful, replacing, backreference-supporting regex lib yet.
        --
        -- 3. use sed. Have to assume non-GNU sed, eg on mac.

        -- Things to update in package.yaml:
        --
        --  version: VER
        cmd_ Shell sed "-i -e" ("'s/(^version *:).*/\\1 "++version++"/'") out
        --
        --  -DVERSION="VER"
        cmd_ Shell sed "-i -e" ("'s/(-DVERSION=)\"[^\"]+/\\1\""++version++"/'") out
        --
        --  this package's dependencies on other hledger packages (typically hledger-lib, hledger)
        --
        --  This one is a bit tricky, and we do it with these limitations:
        --  a. We handle bounds in one of these forms (allowing extra whitespace):
        --     ==A
        --     >A
        --     >=A
        --     >A && <B
        --     >=A && <B
        --  b. We set
        --     the new lower bound to:         this package's new version, V
        --     the new upper bound if any, to: the next major version after V
        --     both of which may not be what's desired.
        --  c. We convert > bounds to >= bounds.
        --
        --  hledger[-PKG] ==LOWER
        let versionre = "([0-9]+\\.)*[0-9]+"  -- 2 or 3 part version number regexp
        cmd_ Shell sed "-i -e" ("'s/(hledger(-[a-z]+)?) *== *"++versionre++" *$/\\1 == "++version++"/'") out
        --
        --  hledger[-PKG] >[=]LOWER
        cmd_ Shell sed "-i -e" ("'s/(hledger(-[a-z]+)?) *>=? *"++versionre++" *$/\\1 >= "++version++"/'") out
        --
        --  hledger[-PKG] >[=]LOWER && <UPPER
        let
          pat = "(hledger(-[a-z]+)?) *>=? *"++versionre++" *&& *< *"++versionre++" *$"
          rpl = "\\1 >="++version++" \\&\\& <"++nextmajorversion -- This was a beast. These ampersands must be backslash-escaped.
          arg = "'s/"++pat++"/"++rpl++"/'"
        cmd_ Shell sed "-i -e" arg out

        let pkg = takeDirectory out
        when (pkg /= "hledger-lib") $ liftIO $ do
          putStrLn $ out++": hledger bounds are (improve if needed):"
          cmd_ Shell grep "'^ *- +hledger.*[<>=]'" out 
            " || [[ $? == 1 ]]"  -- ignore no matches, https://unix.stackexchange.com/a/427598

      phony "cabalfiles" $ docabalfiles

      -- MANUALS

      -- Generate the manuals in plain text, nroff, info, and markdown formats.
      phony "manuals" $ do
        need $ concat [
               nroffmanuals
              ,infomanuals
              ,txtmanuals
              ,webmanuals
              ]
        when commit $ do
          let msg = ";update manuals"
          cmd Shell gitcommit ("-m '"++msg++"' --") packagemandatem4s nroffmanuals infomanuals txtmanuals

      -- Update the dates to show in man pages, to the current month and year.
      -- Currently must be run manually when needed.
      -- Dates are stored in PKG/.date.m4, and are committed along with manuals by Shake manuals -c.
      phony "mandates" $ do
        date <- chomp . fromStdout <$> (cmd Shell "date +'%B %Y'" :: Action (Stdout String))
        forM_ packagemandatem4s $ \f -> do
          cmd_ Shell ["perl","-pi","-e","'s/(.*)\\{\\{.*}}(.*)$/\\1\\{\\{"++date++"}}\\2/'",f]

      -- Generate nroff man pages suitable for man output, from the .m4.md source.
      -- Also updates the _monthyear_ macro to current month and year in hledger*/.date.m4.
      phony "nroffmanuals" $ need nroffmanuals
      nroffmanuals |%> \out -> do -- hledger/hledger.1
        let src       = manpageNameToManualName out <.> "m4.md"
            commonm4  = "doc/common.m4"
            commandsm4 = "hledger/Hledger/Cli/Commands/commands.m4"
            dir       = takeDirectory out
            pkg       = dir
            packagemanversionm4 = dir </> ".version.m4"
            packagemandatem4 = dir </> ".date.m4"
            tmpl      = "doc/manpage.nroff"
        pkgversion <- liftIO $ readFile $ dir </> ".version"
        -- mandate <- formatTime defaultTimeLocale "%B %Y" <$> liftIO getCurrentDay  -- XXX not using this.. compare with .date.m4
        -- assume any other .m4.md files in dir are included by this one XXX not true in hledger-lib
        subfiles <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
        need $ [src, commonm4, commandsm4, packagemanversionm4, packagemandatem4, tmpl] ++ subfiles
        when (dir=="hledger") $ need commandmds
        cmd Shell
          m4 "-DMAN -I" dir commonm4 commandsm4 packagemanversionm4 packagemandatem4 src "|"
          pandoc fromsrcmd "-s" "--template" tmpl
          ("-V footer='"++pkg++"-"++pkgversion++"'")
          "--lua-filter tools/pandoc-drop-html-blocks.lua"
          "--lua-filter tools/pandoc-drop-html-inlines.lua"
          "--lua-filter tools/pandoc-drop-links.lua"
          "-o" out

      -- Generate plain text manuals suitable for embedding in
      -- executables and viewing with a pager, from the man pages.
      -- (Depends on the nroffmanuals.)
      phony "txtmanuals" $ need txtmanuals
      txtmanuals |%> \out -> do  -- hledger/hledger.txt
        let src = manualNameToManpageName $ dropExtension out
        need [src]
        -- cmd Shell groff "-t -e -mandoc -Tascii" src  "| col -b >" out -- http://www.tldp.org/HOWTO/Man-Page/q10.html
        -- Workaround: groff 1.22.4 always calls grotty in a way that adds ANSI/SGR escape codes.
        -- (groff -c is supposed to switch those to backspaces, which we could
        -- remove with col -b, but it doesn't as can be seen with groff -V.)
        -- To get plain text, we run groff's lower-level commands (from -V) and add -cbuo.
        -- -Wall silences most troff warnings, remove to see them
        cmd Shell "tbl" src "| eqn -Tascii | troff -Wall -mandoc -Tascii | grotty -cbuo >" out

      -- Generate Info manuals suitable for viewing with info, from the .m4.md source.
      phony "infomanuals" $ need infomanuals
      infomanuals |%> \out -> do -- hledger/hledger.info
        let src       = out -<.> "m4.md"
            commonm4  = "doc/common.m4"
            commandsm4 = "hledger/Hledger/Cli/Commands/commands.m4"
            dir       = takeDirectory out
            packagemanversionm4 = dir </> ".version.m4"
            packagemandatem4 = dir </> ".date.m4"
        -- assume any other .m4.md files in dir are included by this one XXX not true in hledger-lib
        subfiles <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
        need $ [src, commonm4, commandsm4, packagemanversionm4, packagemandatem4] ++ subfiles
        when (dir=="hledger") $ need commandmds
        cmd Shell
          m4 "-DINFO -I" dir commonm4 commandsm4 packagemanversionm4 packagemandatem4 src "|"
          -- sed "-e 's/^#(#+)/\\1/'" "|"
          pandoc fromsrcmd
          "--lua-filter tools/pandoc-drop-html-blocks.lua"
          "--lua-filter tools/pandoc-drop-html-inlines.lua"
          "--lua-filter tools/pandoc-drop-links.lua"
          -- add "standalone" headers ? sounds good for setting text encoding,
          -- but messes up quotes ('a' becomes ^Xa^Y)
          -- "-s"
          "-t texinfo |"
          makeinfo "-o" out


      -- WEBSITE MARKDOWN SOURCE

      -- Generate the individual web manuals' markdown source, using m4
      -- and pandoc to tweak content.
      phony "webmanuals" $ need webmanuals
      webmanuals |%> \out -> do -- hledger/hledger.md, hledger/hledger-ui.md ..
        let 
            dir       = takeDirectory out -- hledger, hledger-lib
            manpage   = webManualNameToManpageName $ dropExtension $ dropExtension $ takeFileName out -- hledger, journal
            manual    = manpageNameToManualName manpage -- hledger, hledger_journal
            src       = dir </> manual <.> "m4.md"
            commonm4  = "doc/common.m4"
            commandsm4 = "hledger/Hledger/Cli/Commands/commands.m4"
            packageversionm4 = dir </> ".version.m4"
            packagemandatem4 = dir </> ".date.m4"
            heading   = let h = manual
                        in if "hledger_" `isPrefixOf` h
                           then drop 8 h ++ " format"
                           else h
        -- assume any other .m4.md files in dir are included by this one XXX not true in hledger-lib
        subfiles <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
        let deps = [src, commonm4, commandsm4, packageversionm4, packagemandatem4] ++ subfiles
        need deps
        when (manual=="hledger") $ need commandmds
        -- add the web page's heading.
        -- XXX Might be nice to do this atomically with the below, so
        -- make avoid any double refresh when watch docs with entr/livereload.
        -- But cmd Shell doesn't handle arguments containing spaces properly.
        liftIO $ writeFile out $ unlines [
           "<!-- " ++ "Generated by \"Shake webmanuals\" from " ++ unwords deps ++ " -->"
          ,"<div class=\"docversions\"></div>"
          ,"<div class=\"pagetoc\">"
          ,"<!-- toc -->"
          ,"</div>"
          ,""
          ,"# " ++ heading
          ,""
          ]
        cmd Shell
          m4 "-DWEB -I" dir commonm4 commandsm4 packageversionm4 packagemandatem4 src "|"
          pandoc fromsrcmd towebmd
          "--lua-filter tools/pandoc-demote-headers.lua"
          ">>" out

      -- This rule, for updating the live hledger.org site, gets called by:
      -- 1. github-post-receive (github webhook handler), when something is pushed
      --    to the main repo on Github. Config:
      --     /etc/supervisord.conf -> [program:github-post-receive]
      --     /etc/github-post-receive.conf
      -- 2. cron, nightly. Config: /etc/crontab
      -- 3. manually (make site).
      -- phony "hledgerorg" $ do
      --   -- XXX ideally we would ensure here that output is logged in site.log,
      --   -- but I don't know how to do that for the Shake rules.
      --   -- Instead we'll do the logging in "make site".
      --   cmd_ Shell
      --
      --     -- print timestamp. On mac, use brew-installed GNU date.
      --     "PATH=\"/usr/local/opt/coreutils/libexec/gnubin:$PATH\" date --rfc-3339=seconds"
      --     -- pull latest code and site repos - sometimes already done by webhook, not always
      --     "&& printf 'code repo: ' && git pull"
      --     "&& printf 'site repo: ' && git -C site pull"
      --
      --   -- Shake.hs might have been updated, but we won't execute the
      --   -- new one, too insecure. Continue with this one.
      --
      -- Help:
      -- ,"./Shake hledgerorg       update the hledger.org website (when run on prod)"

      -- HLEDGER PACKAGES/EXECUTABLES

      -- build [PKGS]
      -- Build some or all hledger packages, after generating any doc
      -- files they embed or import.
      -- This may also update .cabal files from package.yaml files, and/or install haskell deps.
      phony "build" $ do
        let
          pkgs | null args = packages
               | otherwise = args
        sequence_ [ do
          need $ fromMaybe [] $ lookup pkg embeddedFiles
          cmd Shell "stack build " pkg :: Action ()
          | pkg <- pkgs
          ]

      -- regenerate Hledger/Cli/Commands/*.txt from the .md source files for CLI help
      phony "cmdhelp" $ do
        need commandtxts
        when commit $ do
          let msg = ";update CLI usage texts"
          cmd Shell gitcommit ("-m '"++msg++"' --") commandtxts

      commandtxts |%> \out -> do
        let src = out -<.> "md"
        need [src]
        cmd Shell
          pandoc fromsrcmd src "--lua-filter" "tools/pandoc-dedent-code-blocks.lua" "-t plain" ">" out

      -- CHANGELOGS

      let
        -- git log showing short commit hashes
        gitlog = "git log --abbrev-commit"

        -- git log formats suitable for changelogs/release notes
        -- %s=subject, %an=author name, %n=newline if needed, %w=width/indent1/indent2, %b=body, %h=hash
        changelogGitFormat = "--pretty=format:'- %s (%an)%n%w(0,2,2)%b\n'"
        -- changelogVerboseGitFormat = "--pretty=format:'- %s (%an)%n%w(0,2,2)%b%h' --stat"

        -- Format a git log message, with one of the formats above, as a changelog item
        changelogCleanupCmd = unwords [
           sed
          ,"-e 's/^( )*\\* /\1- /'"        --  ensure bullet lists in descriptions use hyphens not stars
          ,"-e 's/ \\(Simon Michael\\)//'" --  strip maintainer's author name
          ,"-e 's/^- (doc: *)?(updated? *)?changelogs?( *updates?)?$//'"  --  strip some variants of "updated changelog"
          ,"-e 's/^ +\\[ci skip\\] *$//'"  --  strip [ci skip] lines
          ,"-e 's/^ +$//'"                 --  replace lines containing only spaces with empty lines
          -- ,"-e 's/\r//'"                   --  strip windows carriage returns (XXX \r untested. IDEA doesn't like a real ^M here)
          ,"-e '/./,/^$/!d'"               --  replace consecutive newlines with one
          ]

        -- Things to exclude when doing git log for project-wide changelog.
        -- git exclude pathspecs, https://git-scm.com/docs/gitglossary.html#gitglossary-aiddefpathspecapathspec
        projectChangelogExcludeDirs = unwords [
           ":!hledger-lib"
          ,":!hledger"
          ,":!hledger-ui"
          ,":!hledger-web"
          ,":!tests"
          ]

      -- update all changelogs with latest commits
      phony "changelogs" $ do
        need changelogs
        when commit $ do
          let msg = ";update changelogs"
          cmd Shell gitcommit ("-m '"++msg++"' --") changelogs

      -- [PKG/]CHANGES.md
      -- Add any new non-boring commits to the specified changelog, in
      -- an idempotent way, minimising manual toil, as follows. We look at:
      --
      -- - the changelog's topmost markdown heading, which can be a
      --   dev heading (first word is a git revision like 4fffe6e7) or
      --   a release heading (first word is a release version & tag
      --   like 1.18.1, second word is a date like 2020-06-21) or a
      --   package release heading (hledger-ui-1.18.1).
      --
      -- - the package version, in the adjacent .version file, which
      --   can be a dev version like 1.18.99 (first two digits of last
      --   part are 97, 98 or 99) or a release version like 1.18.1
      --   (any other cabal-style version).
      --
      -- The old changelog heading is removed if it was a dev heading;
      -- new commits in PKG not prefixed with semicolon are added;
      -- and a suitable new heading is added: a release heading if
      -- the package version looks like a release version, otherwise 
      -- a dev heading with the current HEAD revision.
      -- 
      -- With -n/--dry-run, print new content to stdout instead of
      -- updating the changelog.
      --
      phonys (\out -> if
        | not $ out `elem` changelogs -> Nothing
        | otherwise -> Just $ do
          tags <- lines . fromStdout <$> (cmd Shell "git tag" :: Action (Stdout String))
          oldlines <- liftIO $ lines <$> readFileStrictly out
          let
            dir = takeDirectory out
            mpkg | dir=="."  = Nothing
                 | otherwise = Just dir
            (preamble, oldheading:rest) = span isnotheading oldlines
              where isnotheading = not . ("#" `isPrefixOf`)
            -- changelog version: a hash or the last release version of this package (or the project)
            changelogversion = headDef err $ drop 1 $ words oldheading
              where err = error $ "could not parse changelog heading: "++oldheading
            -- prepend the package name if we are in a package (not the top-level project directory)
            maybePrependPackage s = maybe s (++("-"++s)) mpkg
            toTag = maybePrependPackage
            isOldRelease rev = isReleaseVersion rev && toTag rev `elem` tags
            isNewRelease rev = isReleaseVersion rev && not (toTag rev `elem` tags)
            -- git revision corresponding to the changelog version:
            -- a hash (a3f19c15), package release tag (hledger-ui-1.20), or project release tag (1.20)
            lastrev
              | isOldRelease changelogversion = toTag changelogversion  -- package release tag
              | isNewRelease changelogversion =
                  trace (out ++ "'s version \""++changelogversion++"\" is not yet tagged, can't list changes")
                  "HEAD"
              | otherwise = changelogversion
                
          -- interesting commit messages between lastrev and HEAD, cleaned up
          let
            interestingpaths = fromMaybe projectChangelogExcludeDirs mpkg
            interestingmessages = "--invert-grep --grep '^;'"  -- ignore commits beginning with ;
          newitems <- fromStdout <$>
                        (cmd Shell gitlog changelogGitFormat (lastrev++"..") interestingmessages "--" interestingpaths
                         "|" changelogCleanupCmd :: Action (Stdout String))

          -- git revision of current HEAD
          headrev <- unwords . words . fromStdout <$>
                     (cmd Shell gitlog "-1 --pretty=%h -- " interestingpaths :: Action (Stdout String))
          -- package version: the version number currently configured for this package (or the project)
          packageversion <-
            let versionfile = dir </> ".version"
                err = error $ "could not parse a version in "++versionfile
            in liftIO $ headDef err . words <$> readFileStrictly versionfile
          date <- liftIO getCurrentDay
          let
            -- the new changelog heading will be a final (dated, versioned) heading if
            -- the configured package version is a new release version (non-dev & non-tagged)
            (newrev, newheading)
              | isNewRelease packageversion = (toTag packageversion, unwords [packageversion, show date])
              | otherwise                   = (headrev, headrev)
            newcontent = "# "++newheading++"\n" ++ newitems
            newchangelog =
                 unlines preamble
              ++ newcontent
              ++ (if isCommitHash changelogversion then "" else oldheading)
              ++ unlines rest

          liftIO $ if
            | lastrev == newrev -> pure ()  -- putStrLn $ out ++ ": up to date"
            | dryrun -> putStr $ out ++ ":\n" ++ newcontent
            | otherwise -> do
                writeFile out newchangelog
                putStrLn $ out ++ ": updated to " ++ newrev

          )

      -- Update all program-specific docs, eg after setversion.
      phony "docs" $ need [
         "cmdhelp"
        ,"manuals"
        ,"changelogs"
        ]

      -- MISC

      -- Generate the web manuals based on the current checkout and save
      -- them as the specified versioned snapshot in site/doc/VER/ .
      -- .snapshot is a dummy file.
      -- "site/doc/*/.snapshot" %> \out -> do
      --   need webmanuals
      --   let snapshot = takeDirectory out
      --   cmd_ Shell "mkdir -p" snapshot
      --   forM_ webmanuals $ \f -> -- site/hledger.md, site/journal.md
      --     cmd_ Shell "cp" f (snapshot </> takeFileName f)
      --   cmd_ Shell "cp -r site/images" snapshot
      --   cmd_ Shell "touch" out
      -- Help:
      -- ,"./Shake site/doc/VERSION/.snapshot  save current web manuals as this snapshot"

      -- Cleanup.

      phony "clean" $ do
        putNormal "Cleaning object files in tools"
        removeFilesAfter "tools"  ["*.o","*.p_o","*.hi"]

      phony "Clean" $ do
        need ["clean"]
        putNormal "Cleaning shake build cache"
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

readFileStrictly :: FilePath -> IO String
readFileStrictly f = readFile f >>= \s -> C.evaluate (length s) >> return s

-- Write new content to a file, only if it's different.
maybeWriteFile :: FilePath -> String -> IO ()
maybeWriteFile f new = do
  old <- readFileStrictly f
  when (old /= new) $ writeFile f new

-- | Get the current local date.
getCurrentDay :: IO Day
getCurrentDay = do
  t <- getZonedTime
  return $ localDay (zonedTimeToLocalTime t)

-- | Replace each occurrence of a regular expression by this string.
replaceRe :: RE -> String -> String -> String
replaceRe re repl = replaceBy re (\_ _ _ -> Just repl)

-- | Replace each occurrence of a regular expression, by transforming
-- each matched text with the given function.
replaceBy :: RE -> (Match String -> RELocation -> Capture String -> Maybe String) -> String -> String
replaceBy re f src = replaceAllCaptures TOP f $ src *=~ re

-- | Does this string look like a valid cabal package version ?
isVersion s = not (null s) && all (`elem` "0123456789.") s

-- | Does this string look like a hledger development version ?
-- Ie a version where the first two digits of the last part are the
-- special values 97, 98 or 99, indicating alpha/beta/rc or whatever.
isDevVersion s = isVersion s && lastpart >= "97"
  where lastpart = lastDef "" $ splitOn "." s

-- | Does this string look like a hledger release version ?
-- Ie a cabal package version that's not a hledger development version.
isReleaseVersion s = isVersion s && not (isDevVersion s)

-- | Does this string look like a git commit hash ?
-- Ie a sequence of 7 or more numbers or letters.
isCommitHash s = length s > 6 && all isAlphaNum s

-- | Remove all trailing newlines/carriage returns.
chomp :: String -> String
chomp = reverse . dropWhile (`elem` "\r\n") . reverse

