#!/usr/bin/env stack
{- stack exec
   --verbosity=info
   --package base-prelude
   --package directory
   --package extra
   --package regex
   --package safe
   --package shake
   --package time
   ghc
-} -- uses the project's current stack resolver
{-

One of two project scripts files (Makefile, Shake.hs). This one
provides a stronger programming language and more platform
independence than Make. It requires stack and will auto-install the
haskell packages above when needed (on first run or when a new
resolver is configured in stack.yaml). Some rules below use additional
tools, including:

- groff
- m4
- makeinfo
- pandoc
- sed
- GNU date (on mac: brew install coreutils)

Compiling this script is recommended, to ensure required packages are
installed, minimise startup delay, and reduce sensitivity to the
current git state (eg when bisecting). To compile, run "./Shake.hs".
(Or "make Shake", or any other make rule depending on Shake).

Once compiled, run ./Shake without any arguments to list commands and
targets (see below).

When developing/troubleshooting this script, these are useful:

watch Shake.hs for compile errors: make ghcid-shake
load Shake.hs in GHCI: make ghci-shake
rebuild things when files change with entr (file watcher), eg:
 find hledger-lib hledger | entr ./Shake manuals
view rule dependency graph:
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
import "extra"        Data.List.Extra
import "process"      System.Process
import "regex"        Text.RE.TDFA.String
import "regex"        Text.RE.Replace
import "safe"         Safe
import "shake"        Development.Shake
import "shake"        Development.Shake.FilePath
import "time"         Data.Time
-- import "hledger-lib"  Hledger.Utils.Debug

usage = unlines
    ---------------------------------------79--------------------------------------
  ["Usage:"
  ,"./Shake.hs               (re)compile this script"
  ,"./Shake commandhelp      build help texts for the hledger CLI"
  ,"./Shake manuals          build txt/man/info/web manuals for all packages"
  -- ,"./Shake htmlmanuals      build html manuals for all packages"
  -- ,"./Shake oldmanuals       build old versions of html manuals for all packages"
  ,"./Shake PKG              build a single hledger package and its embedded docs"
  ,"./Shake build            build all hledger packages and their embedded docs"
  -- ,"./Shake website          build the website and web manuals"
  -- ,"./Shake website-all      build the website and all web manual versions"
  ,"./Shake all              build all the above"
  -- ,"./Shake hledgerorg       update the hledger.org website (when run on prod)"
  ,""
  -- ,"./Shake mainpages                   build the web pages from the main repo"
  -- ,"./Shake site/index.md               update wiki links on the website home page"
  ,"./Shake FILE                        build any individual file"
  ,"./Shake setversion                  update all packages from PKG/.version"
  ,"./Shake changelogs                  update the changelogs with any new commits"
  ,"./Shake [PKG/]CHANGES.md[-dry]      update or preview this changelog"
  ,"./Shake [PKG/]CHANGES.md-finalise   set final release heading in this changelog"
  -- ,"./Shake site/doc/VERSION/.snapshot  save current web manuals as this snapshot"
  ,""
  ,"./Shake clean            clean help texts, manuals, staged site content"
  ,"./Shake Clean            also clean rendered site, object files, Shake's cache"
  ,"./Shake [help]           show these commands"
  ,"./Shake --help           show Shake options (--color, --rebuild, ...)"
  ,""
  ,"See also: make help"
  ]

-- groff    = "groff -c" ++ " -Wall"  -- see "groff" below
makeinfo = "makeinfo" ++ " --no-warn"  -- silence makeinfo warnings - comment out to see them
pandoc   = "pandoc"

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
towebmd = "-t markdown-smart-fenced_divs-fenced_code_attributes-simple_tables-multiline_tables-grid_tables --atx-headers"


main = do

  -- hledger manual also includes the markdown files from here:
  let commandsdir = "hledger/Hledger/Cli/Commands"
  commandmds <-
    filter (not . ("README." `isPrefixOf`) . takeFileName) . filter (".md" `isSuffixOf`) . map (commandsdir </>)
    <$> S.getDirectoryContents commandsdir
  let commandtxts = map (-<.> "txt") commandmds

  let sitedir = "site"
  pages <- map takeBaseName . filter (".md" `isSuffixOf`) <$> S.getDirectoryContents sitedir

  shakeArgs
    shakeOptions
      {
       shakeVerbosity=Quiet
      -- ,shakeReport=[".shake.html"]
      }
      $ do

    want ["help"]

    phony "help" $ liftIO $ putStrLn usage

    phony "all" $ need ["commandhelp", "manuals", "build"]  --, "website"]

    -- phony "compile" $ need ["Shake"]
    -- "Shake" %> \out -> do
    --   need [out <.> "hs"]
    --   unit $ cmd "./Shake.hs"  -- running as stack script installs deps and compiles
    --   putLoud "You can now run ./Shake instead of ./Shake.hs"


    -- NAMES, FILES, URIS..

    let
      -- documentation versions shown on the website
      docversions = [ "1.0" , "1.1" , "1.2" , "1.3" , "1.4" , "1.5" , "1.9", "1.10", "1.11", "1.12", "1.13", "1.14", "1.15" ]

      -- main package names, in standard build order
      packages = [
         "hledger-lib"
        ,"hledger"
        ,"hledger-ui"
        ,"hledger-web"
        ]

      changelogs = "CHANGES.md" : map (</> "CHANGES.md") packages

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

      -- manuals as web-ready markdown, written into the website for Sphinx (site/hledger.md)
      webmanuals = ["site" </> manpageNameToUri m <.> "md" | m <- manpageNames]

      -- -- latest version of the manuals rendered to html (site/_site/hledger.html)
      -- htmlmanuals = ["site/_site" </> manpageNameToUri m <.> "html" | m <- manpageNames]

      -- -- old versions of the manuals rendered to html (site/_site/doc/1.14/hledger.html)
      -- oldhtmlmanuals = map (normalise . ("site/_site/doc" </>) . (<.> "html")) $
      --   [ v </> manpageNameToUri p | v <- docversions, v>="1.0", p <- manpageNames ++ ["manual"] ] ++
      --   [ v </> "manual"           | v <- docversions, v <"1.0" ]  -- before 1.0 there was only the combined manual

      -- the html for website pages kept in the main repo
      -- mainpageshtml = map (normalise . ("site/_site" </>) . (<.> "html")) pages

      -- TODO: make website URIs lower-case ?

      -- extensions of static web asset files, to be copied to the website
      -- webassetexts = ["png", "gif", "cur", "js", "css", "eot", "ttf", "woff", "svg"]

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

    -- MANUALS

    -- Generate the manuals in plain text, nroff, info, and markdown formats.
    phony "manuals" $ need $
      "commandhelp" :
      concat [
       nroffmanuals
      ,infomanuals
      ,txtmanuals
      ,webmanuals
      ]

    -- Generate nroff man pages suitable for man output.
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
    -- executables and viewing with a pager.
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

    -- Generate Info manuals suitable for viewing with info.
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
        "m4 -P -I" dir commonm4 packagem4 src "|"
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
      let manpage   = manpageUriToName $ dropExtension $ takeFileName out -- hledger
          manual    = manpageNameToManualName manpage
          dir       = manpageDir manpage
          src       = dir </> manual <.> "m4.md"
          commonm4  = "doc/common.m4"
          packagem4 = dir </> "defs.m4"
          heading   = let h = manual
                      in if "hledger_" `isPrefixOf` h
                         then drop 8 h ++ " format"
                         else h
      -- assume all other m4 files in dir are included by this one XXX not true in hledger-lib
      deps <- liftIO $ filter (/= src) . filter (".m4.md" `isSuffixOf`) . map (dir </>) <$> S.getDirectoryContents dir
      need $ [src, commonm4, packagem4] ++ deps
      when (manual=="hledger") $ need commandmds
      liftIO $ writeFile out $ "# " ++ heading ++ "\n\n"
      cmd Shell
        "m4 -P -DMAN -DWEB -I" dir commonm4 packagem4 src "|"
        pandoc fromsrcmd towebmd
        "--lua-filter tools/pandoc-demote-headers.lua"
        ">>" out

    -- Copy some extra markdown files from the main repo into the site
    -- TODO adding table of contents placeholders
    -- [
    --   -- "site/README.md",
    --   -- "site/CONTRIBUTING.md"
    --   ]  |%> \out ->
    --   copyFile' (dropDirectory1 out) out -- XXX (map toLower out)

    -- WEBSITE HTML & ASSETS

    -- phony "website" $ need [
    --    "webassets"
    --   -- ,"mainpages"
    --   -- ,"htmlmanuals"
    --   ]

    -- phony "website-all" $ need [
    --    "website"
    --   -- ,"oldmanuals"
    --   ]

    -- -- copy all static asset files (files with certain extensions
    -- -- found under sites, plus one or two more) to sites/_site/
    -- phony "webassets" $ do
    --     assets <- getDirectoryFiles "site" (map ("//*" <.>) webassetexts)
    --     need [ "site/_site" </> file
    --             | file <- assets ++ [
    --                 "files/README"
    --                 ]
    --             , not ("_site//*" ?== file)
    --          ]

    -- copy any one of the static asset files to sites/_site/
    -- "site/_site/files/README" : [ "site/_site//*" <.> ext | ext <- webassetexts ] |%> \out -> do
    --     copyFile' ("site" </> dropDirectory2 out) out

    -- render all web pages from the main repo (manuals, home, download, relnotes etc) as html, saved in site/_site/
    -- phony "mainpages" $ need mainpageshtml

    -- phony "htmlmanuals" $ need htmlmanuals

    -- phony "oldmanuals" $ need oldhtmlmanuals

    -- Render one website page as html, saved in sites/_site/.
    -- Github-style wiki links will be hyperlinked.
    -- "site/_site//*.html" %> \out -> do
    --     let filename = takeBaseName out
    --         pagename = fileNameToPageName filename
    --         isdownloadpage = filename == "download"
    --         isoldmanual = "site/_site/doc/" `isPrefixOf` out
    --         source
    --           | isoldmanual = "site" </> (drop 11 $ dropExtension out) <.> "md"
    --           | otherwise   = "site" </> filename <.> "md"
    --         template = "site/site.tmpl"
    --         siteRoot = if "site/_site/doc//*" ?== out then "../.." else "."
    --     need [source, template]
    --     -- read markdown source, link any wikilinks, pipe it to pandoc, write html out
    --     Stdin . wikiLink <$> (readFile' source) >>=
    --       (cmd Shell pandoc "-" fromsrcmd "-t html"
    --                        "--template" template
    --                        ("--metadata=siteRoot:" ++ siteRoot)
    --                        ("--metadata=\"title:" ++ pagename ++ "\"")
    --                        "-o" out )

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

    --     -- print timestamp. On mac, use brew-installed GNU date.
    --     "PATH=\"/usr/local/opt/coreutils/libexec/gnubin:$PATH\" date --rfc-3339=seconds"
    --     -- pull latest code and site repos - sometimes already done by webhook, not always
    --     "&& printf 'code repo: ' && git pull"
    --     "&& printf 'site repo: ' && git -C site pull"

    --   -- Shake.hs might have been updated, but we won't execute the
    --   -- new one, too insecure. Continue with this one.

    --   -- update the live site based on all latest content
    --   need [ "website-all" ]

    -- HLEDGER PACKAGES/EXECUTABLES

    phony "build" $ need packages

    -- build any of the hledger packages, after generating any doc
    -- files they embed or import.
    sequence_ [ phony pkg $ do
      need $ fromMaybe [] $ lookup pkg embeddedFiles
      cmd Shell "stack build " pkg
      | pkg <- packages ]

    phony "commandhelp" $ need commandtxts

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

    -- show the changelogs updates that would be written
    -- phony "changelogs-dry" $ need changelogsdry

    -- [PKG/]CHANGES.md[-dry] <- git log
    -- Add commits to the specified changelog since the tag/commit in
    -- the topmost heading, also removing that previous heading if it
    -- was an interim heading (a commit hash). Or (the -dry variants)
    -- just print the new changelog items to stdout without saving.
    phonys (\out' -> if
      | not $ out' `elem` (changelogs ++ map (++"-dry") changelogs) -> Nothing
      | otherwise -> Just $ do
        let (out, dryrun) | "-dry" `isSuffixOf` out' = (take (length out' - 4) out', True)
                          | otherwise                = (out', False)
        old <- liftIO $ lines <$> readFileStrictly out

        let dir = takeDirectory out
            pkg | dir=="."  = Nothing
                | otherwise = Just dir
            gitlogpaths = fromMaybe projectChangelogExcludeDirs pkg
            isnotheading = not . ("#" `isPrefixOf`)
            iscommithash s = length s > 6 && all isAlphaNum s
            (preamble, oldheading:rest) = span isnotheading old
            lastversion = words oldheading !! 1
            lastrev | iscommithash lastversion = lastversion
                    | otherwise                = fromMaybe "hledger" pkg ++ "-" ++ lastversion
            excludeboring = "--invert-grep --grep '^;'"  -- ignore commits beginning with ;

        headrev <- unwords . words . fromStdout <$>
                   (cmd Shell gitlog "-1 --pretty=%h -- " gitlogpaths :: Action (Stdout String))

        if headrev == lastrev
        then liftIO $ putStrLn $ out ++ ": up to date"
        else do
          newitems <- fromStdout <$>
                        (cmd Shell gitlog changelogGitFormat (lastrev++"..") excludeboring "--" gitlogpaths
                         "|" changelogCleanupCmd :: Action (Stdout String))
          let newcontent = "# "++headrev++"\n\n" ++ newitems
              newfile = unlines $ concat [
                 preamble
                ,[newcontent]
                ,if iscommithash lastrev then [] else [oldheading]
                ,rest
                ]
          liftIO $ if dryrun
                   then putStr newcontent
                   else do
                     writeFile out newfile
                     putStrLn $ out ++ ": updated to " ++ headrev
        )

    -- [PKG/]CHANGES.md-finalise <- PKG/.version
    -- Converts the specified changelog's topmost heading, if it is an
    -- interim heading (a commit hash), to a permanent heading
    -- containing the intended release version (from .version) and
    -- today's date.  For the project CHANGES.md, the version number
    -- in hledger/.version is used.
    phonys (\out' -> let suffix = "-finalise" in if
      | not $ out' `elem` (map (++suffix) changelogs) -> Nothing
      | otherwise -> Just $ do
        let
          out = take (length out' - length suffix) out'
          versiondir = case takeDirectory out of
                         "." -> "hledger"
                         d   -> d
          versionfile = versiondir </> ".version"
        need [versionfile]
        version <- ((head . words) <$>) $ liftIO $ readFile versionfile
        old     <- liftIO $ readFileStrictly out
        date    <- liftIO getCurrentDay
        let (before, _:after) = break ("# " `isPrefixOf`) $ lines old
            new = unlines $ before ++ ["# "++version++" "++show date] ++ after
        liftIO $ do
          writeFile out new
          putStrLn $ out ++ ": updated to " ++ version
        )

    -- VERSION NUMBERS

    -- Given the desired version string saved in PKG/.version, update
    -- it everywhere needed in the package. See also CONTRIBUTING.md >
    -- Version numbers.

    let inAllPackages f = map (</> f) packages

    phony "setversion" $ need $
         inAllPackages "defs.m4"
      ++ inAllPackages "package.yaml"

    -- PKG/defs.m4 <- PKG/.version
    "hledger*/defs.m4" %> \out -> do
      let versionfile = takeDirectory out </> ".version"
      need [versionfile]
      version <- ((head . words) <$>) $ liftIO $ readFile versionfile
      cmd_ Shell sed "-i -e" ("'s/(_version_}}, *)\\{\\{[^}]+/\\1{{"++version++"/'") out

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

-- markdown helpers

type Markdown = String

-- | Prepend a markdown heading.
addHeading :: String -> Markdown -> Markdown
addHeading h = (("# "++h++"\n\n")++)

-- | Convert Github-style wikilinks to hledger website links.
wikiLink :: Markdown -> Markdown
wikiLink =
  replaceBy wikilinkre         wikilinkReplace         .
  replaceBy labelledwikilinkre labelledwikilinkReplace

-- regex stuff

-- couldn't figure out how to use match subgroups, so we don't
-- wikilinkre         = [re|\[\[$([^]]+)]]|]                -- [[A]]
-- labelledwikilinkre = [re|\[\[$([^(|)]+)\|$([^]]*)\]\]|]  -- [[A|B]]
wikilinkre         = [re|\[\[[^]]+]]|]             -- [[A]]
labelledwikilinkre = [re|\[\[[^(|)]+\|[^]]*\]\]|]  -- [[A|B]]. The | is parenthesised to avoid ending the quasiquoter

-- wikilinkReplace _ loc@RELocation{locationCapture} cap@Capture{capturedText} =
wikilinkReplace _ _ Capture{capturedText} =
  -- trace (show (loc,cap)) $
  Just $ "["++name++"]("++uri++")"
  where
    name = init $ init $ drop 2 capturedText
    uri  = pageNameToUri name

-- labelledwikilinkReplace _ loc@RELocation{locationCapture} cap@Capture{capturedText} =
labelledwikilinkReplace _ _ Capture{capturedText} =
  Just $ "["++label++"]("++uri++")"
  where
    [label,name] = take 2 $ (splitOn "|" $ init $ init $ drop 2 capturedText) ++ [""]
    uri = pageNameToUri name

pageNameToUri = (++".html") . intercalate "-" . words

fileNameToPageName = unwords . splitOn "-"

-- | Easier regex replace helper. Replaces each occurrence of a
-- regular expression in src, by transforming each matched text with
-- the given function.
replaceBy re f src = replaceAllCaptures TOP f $ src *=~ re

-- not powerful enough, saved for reference:
-- wikify = (*=~/ wikilinkreplace) . (*=~/ labelledwikilinkreplace)
--   where
--     -- [[A]] -> [A](.../A)
--     wikilinkreplace :: SearchReplace RE String
--     wikilinkreplace = [ed|\[\[$([^]]+)]]///[$1]($1.html)|]
--     -- [[A|B]] -> [A](.../B)
--     labelledwikilinkreplace :: SearchReplace RE String
--     labelledwikilinkreplace = [ed|\[\[$([^(|)]+)\|$([^]]*)\]\]///[$1]($2.html)|]
