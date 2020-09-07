#!/usr/bin/env stack
{- stack script --compile --resolver=lts-16.12
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
  ,"./Shake setversion [VER] [PKGS]"
  ,"                         update version strings from */.version (or VER)"
  ,"./Shake commandtxts      update hledger CLI commands' usage texts"
  ,"./Shake manuals          update txt/man/info/web manuals for all packages"
  ,"./Shake webmanuals       update just the web manuals"
  ,"./Shake changelogs [--dry-run]"
  ,"                         add new commits & headings to */CHANGES.md"
  ,"./Shake cabalfiles       update */*.cabal from */package.yaml"
  ,"./Shake build [PKGS]     build hledger packages and their embedded docs"
  ,"./Shake clean            remove generated texts, manuals"
  ,"./Shake Clean            also remove object files, Shake's cache"
  ,"./Shake FILE             build any individual file"
  ,"./Shake --help           list Shake's options (--color, --rebuild, etc.)"
  ,"Keep Shake option arguments adjacent to their flag."
  ]

-- groff    = "groff -c" ++ " -Wall"  -- see "groff" below
makeinfo = "makeinfo" ++ " --no-warn"  -- silence makeinfo warnings - comment out to see them
pandoc   = "pandoc --strip-comments"

-- Must support both BSD sed and GNU sed. Tips:
-- BSD:
-- use [a-z] [0-9] instead of \w \d etc.
-- GNU:
-- backslash-escape {
sed      = "sed -E"

-- The kind of markdown used in our doc source files.
fromsrcmd = "-f markdown-smart-tex_math_dollars"

-- The kind of markdown we like to generate for the website.
-- This will be consumed by sphinx extensions:
--  recommonmark (Commonmark syntax, https://spec.commonmark.org)
--  sphinx-markdown-tables (PHP Markdown Extra table syntax, https://michelf.ca/projects/php-markdown/extra/#table)
-- XXX trying to force the use of pipe_tables here, but sometimes it uses html instead
towebmd = "-t markdown-smart-fenced_divs-fenced_code_attributes-simple_tables-multiline_tables-grid_tables-raw_attribute --atx-headers"


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

      -- NAMES, FILES, URIS..

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
        cabalfiles = [p <.> "cabal" | p <- packages]
        changelogs = map (</> "CHANGES.md") pkgandprojdirs

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

        -- manuals as plain text, ready for embedding as CLI help (hledger/hledger.txt)
        txtmanuals = [manualDir m </> m <.> "txt" | m <- manualNames]

        -- manuals as nroff, ready for man (hledger/hledger.1)
        nroffmanuals = [manpageDir m </> m | m <- manpageNames]

        -- manuals as info, ready for info (hledger/hledger.info)
        infomanuals = [manualDir m </> m <.> "info" | m <- manualNames]

        -- manuals as sphinx-ready markdown, to be rendered as part of the website (hledger/hledger.webmanual.md)
        webmanuals = [manualDir m </> m <.> "webmanual.md" | m <- manualNames]

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

      -- MANUALS

      -- Generate the manuals in plain text, nroff, info, and markdown formats.
      phony "manuals" $ need $
        concat [
         nroffmanuals
        ,infomanuals
        ,txtmanuals
        ,webmanuals
        ]

      -- Generate nroff man pages suitable for man output, from the .m4.md source.
      phony "nroffmanuals" $ need nroffmanuals
      nroffmanuals |%> \out -> do -- hledger/hledger.1
        let src       = manpageNameToManualName out <.> "m4.md"
            commonm4  = "doc/common.m4"
            dir       = takeDirectory out
            packagem4 = dir </> "defs.m4"
            tmpl      = "doc/manpage.nroff"
        -- assume all other m4 files in dir are included by this one XXX not true in hledger-lib
        deps <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
        need $ [src, commonm4, packagem4, tmpl] ++ deps
        when (dir=="hledger") $ need commandmds
        cmd Shell
          "m4 -P -DMAN -I" dir commonm4 packagem4 src "|"
          pandoc fromsrcmd "-s" "--template" tmpl
          "--lua-filter tools/pandoc-drop-html-blocks.lua"
          "--lua-filter tools/pandoc-drop-html-inlines.lua"
          "--lua-filter tools/pandoc-drop-links.lua"
          "-o" out

      -- Generate plain text manuals suitable for embedding in
      -- executables and viewing with a pager, from the man pages.
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
            dir       = takeDirectory out
            packagem4 = dir </> "defs.m4"
        -- assume all other m4 files in dir are included by this one XXX not true in hledger-lib
        deps <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
        need $ [src, commonm4, packagem4] ++ deps
        when (dir=="hledger") $ need commandmds
        cmd Shell
          "m4 -P -DINFO -I" dir commonm4 packagem4 src "|"
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
      webmanuals |%> \out -> do -- hledger/hledger.webmanual.md, hledger-lib/journal.webmanual.md
        let 
            dir       = takeDirectory out -- hledger, hledger-lib
            manpage   = webManualNameToManpageName $ dropExtension $ dropExtension $ takeFileName out -- hledger, journal
            manual    = manpageNameToManualName manpage -- hledger, hledger_journal
            src       = dir </> manual <.> "m4.md"
            commonm4  = "doc/common.m4"
            packagem4 = dir </> "defs.m4"
            heading   = let h = manual
                        in if "hledger_" `isPrefixOf` h
                           then drop 8 h ++ " format"
                           else h
        -- assume any other m4 files in dir are included by this one XXX not true in hledger-lib
        subfiles <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
        let deps = [src, commonm4, packagem4] ++ subfiles
        need deps
        when (manual=="hledger") $ need commandmds
        -- add the web page's heading.
        -- XXX Might be nice to do this atomically with the below, so
        -- make avoid any double refresh when watch docs with entr/livereload.
        -- But cmd Shell doesn't handle arguments containing spaces properly.
        liftIO $ writeFile out $ unlines [
           "<!-- " ++ "Generated by \"Shake webmanuals\" from " ++ unwords deps ++ " -->"
          ,""
          ,"# " ++ heading
          ,""
          ]
        cmd Shell
          "m4 -P -DWEB -I" dir commonm4 packagem4 src "|"
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
      phony "build" $ do
        let
          pkgs | null args = packages
               | otherwise = args
        sequence_ [ do
          need $ fromMaybe [] $ lookup pkg embeddedFiles
          cmd Shell "stack build " pkg :: Action ()
          | pkg <- pkgs
          ]

      -- regenerate .cabal files from package.yaml's, using stack (also installs deps)
      phony "cabalfiles" $ do
        cmd Shell "stack build --dry-run" :: Action ()

      -- regenerate Hledger/Cli/Commands/*.txt from the .md source files for CLI help
      phony "commandtxts" $ need commandtxts

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
        changelogGitFormat = "--pretty=format:'- %s (%an)%n%w(0,2,2)%b'"
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
      phony "changelogs" $ need changelogs

      -- [PKG/]CHANGES.md
      -- Add any new non-boring commits to the specified changelog, in
      -- an idempotent way, minimising manual toil, as follows. We look at:
      --
      -- - the changelog's topmost markdown heading, which can be a
      --   dev heading (first word is a git revision like 4fffe6e7) or
      --   a release heading (first word is a release version & tag
      --   like 1.18.1, second word is a date like 2020-06-21).
      --
      -- - the package version, in the adjacent .version file, which
      --   can be a dev version like 1.18.99 (first two digits of last
      --   part are 97, 98 or 99) or a release version like 1.18.1
      --   (any other cabal-style version).
      --
      -- The old changelog heading is removed if it was a dev heading;
      -- new commits in PKG not prefixed with semicolon are added;
      -- and a suitable new heading is added (a release heading if the
      -- package has a release version set, otherwise a dev heading
      -- with the current HEAD revision).
      -- 
      -- With --dry-run, print new content to stdout instead of
      -- updating the changelog.
      --
      phonys (\out -> if
        | not $ out `elem` changelogs -> Nothing
        | otherwise -> Just $ do
          oldlines <- liftIO $ lines <$> readFileStrictly out
          let
            (preamble, oldheading:rest) = span isnotheading oldlines
              where isnotheading = not . ("#" `isPrefixOf`)
            changelogversion = headDef err $ drop 1 $ words oldheading
              where err = error $ "could not parse changelog heading: "++oldheading
            dir = takeDirectory out

          packageversion <-
            let versionfile = dir </> ".version"
                err = error $ "could not parse a version in "++versionfile
            in (liftIO $ headDef err . words <$> readFileStrictly versionfile)
          let
            pkg | dir=="."  = Nothing
                | otherwise = Just dir
            gitlogpaths = fromMaybe projectChangelogExcludeDirs pkg
            lastrev = changelogversion
          headrev <- unwords . words . fromStdout <$>
                     (cmd Shell gitlog "-1 --pretty=%h -- " gitlogpaths :: Action (Stdout String))
          let excludeboring = "--invert-grep --grep '^;'"  -- ignore commits beginning with ;
          newitems <- fromStdout <$>
                        (cmd Shell gitlog changelogGitFormat (lastrev++"..") excludeboring "--" gitlogpaths
                         "|" changelogCleanupCmd :: Action (Stdout String))
          date <- liftIO getCurrentDay

          let
            (newrev, newheading)
              | isReleaseVersion packageversion = (packageversion, unwords [packageversion, show date])
              | otherwise                       = (headrev, headrev)
            newcontent = "# "++newheading++"\n\n" ++ newitems
            newchangelog = unlines $ concat [
               preamble
              ,[newcontent]
              ,if isCommitHash changelogversion then [] else [oldheading]
              ,rest
              ]
            dryrun = any (`elem` ruleopts) ["--dry-run", "--dry", "-n"]

          liftIO $ if
            | lastrev == newrev -> putStrLn $ out ++ ": up to date"
            | dryrun -> putStr $ out ++ ":\n" ++ newcontent
            | otherwise -> do
                writeFile out newchangelog
                putStrLn $ out ++ ": updated to " ++ newrev

          )

      -- VERSION NUMBERS

      -- Update all packages' version strings based on the version saved in PKG/.version.
      -- If a version number is provided as first argument, save that in the .version files first.
      -- If one or more subdirectories are provided as arguments, save/update only those.
      -- See also CONTRIBUTING.md > Version numbers.
      phony "setversion" $ do
        let
          (mver, dirargs) = (headMay ver', drop 1 ver' ++ dirs')
            where (ver',dirs') = span isVersion ruleargs
          (specifieddirs, specifiedpkgs) =
            case dirargs of [] -> (pkgandprojdirs, pkgdirs)
                            ds -> (ds, ds)
        -- if a version was provided, update .version files in the specified directories
        case mver of
          Just v  -> liftIO $ forM_ specifieddirs $ \d -> writeFile (d </> ".version") (v++"\n")
          Nothing -> return ()

        -- update all files depending on .version in the specified packages
        need $ concat [
           map (</> "defs.m4")      specifiedpkgs
          ,map (</> "package.yaml") specifiedpkgs
          ]

      -- PKG/defs.m4 <- PKG/.version
      "hledger*/defs.m4" %> \out -> do
        let versionfile = takeDirectory out </> ".version"
        need [versionfile]
        version <- ((head . words) <$>) $ liftIO $ readFile versionfile
        date    <- liftIO getCurrentDay
        let manualdate = formatTime defaultTimeLocale "%B %Y" date
        cmd_ Shell sed "-i -e" (
            "'s/(_version_}}, *)\\{\\{[^}]+/\\1{{"++version++"/;"
          ++" s/(_monthyear_}}, *)\\{\\{[^}]+/\\1{{"++manualdate++"/;"
          ++"'")
          out

      -- PKG/package.yaml <- PKG/.version
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

        -- tagrelease: \
        --   $(call def-help,tagrelease, commit a release tag based on $(VERSIONFILE) for each package )
        --   for p in $(PACKAGES); do git tag -f $$p-$(VERSION); done

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
