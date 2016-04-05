#!/usr/bin/env stack
{- stack runghc
   --package base-prelude
   --package directory
   --package extra
   --package here
   --package safe
   --package shake
   --package time
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
  want
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
 ./Shake.hs compile                     # compile this script (optional)
 ./Shake                                # show commands
 ./Shake --help                         # show options
 ./Shake [--color] COMMAND

Commands:
 compile
 manpages
|]

manpages :: [String]
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

manpageDir :: String -> FilePath
manpageDir p
  | '_' `elem` p = "hledger-lib"
  | otherwise    = dropExtension p

buildDir :: FilePath
buildDir = ".build"

pandocExe :: String
pandocExe = "pandoc"

pandocFiltersResolver :: String
pandocFiltersResolver = "--resolver lts-5.11"

main :: IO ()
main = do

  pandocFilters <-
    map ("tools" </>). nub . sort . map (-<.> "") . filter ("pandoc" `isPrefixOf`)
    <$> S.getDirectoryContents "tools"

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

    -- man pages, still markdown but with man-only sections removed.
    -- (We let hakyll do the html rendering since it's good
    -- at applying the site style, table of contents etc.)
    let manpageFilteredMds = ["site" </> p <.>".md" | p <- manpages]

    -- man pages, converted to man nroff with web-only sections removed
    let manpageNroffs = [manpageDir p </> p | p <- manpages]

    phony "manpages" $ need $ manpageNroffs ++ manpageFilteredMds

    manpageNroffs |%> \out -> do
      let
        md = out <.> "md"
        tmpl = "doc/manpage.nroff"
      need $ md : tmpl : pandocFilters
      cmd pandocExe md "--to man -s --template" tmpl
        "--filter tools/pandocRemoveHtmlBlocks"
        "--filter tools/pandocRemoveHtmlInlines"
        "--filter tools/pandocRemoveLinks"
        "--filter tools/pandocRemoveNotes"
        "--filter tools/pandocCapitalizeHeaders"
        "-o" out

    manpageFilteredMds |%> \out -> do
      let
        p = dropExtension $ takeFileName out
        md = manpageDir p </> p <.> "md"
        tmpl = "doc/manpage.html"
      need $ md : tmpl : pandocFilters
      cmd "pandoc" md "--to markdown"
        -- XXX assume this is compiled
        "--filter tools/pandocRemoveManonlyBlocks"
        "-o" out

    pandocFilters |%> \out -> do
      need [out <.> "hs"]
      cmd ("stack "++pandocFiltersResolver++" ghc") out

    phony "clean" $ do
      putNormal "Cleaning generated files"
      removeFilesAfter "" manpageNroffs
      removeFilesAfter "" manpageFilteredMds
      putNormal "Cleaning object files"
      removeFilesAfter "tools" ["*.o","*.p_o","*.hi"]
      putNormal "Cleaning shake build files"
      removeFilesAfter buildDir ["//*"]

    -- manpageHtmls |%> \out -> do
    --   let
    --     p = dropExtension $ takeFileName out
    --     md = manpageDir p </> p <.> "md"
    --     tmpl = "doc/manpage.html"
    --   need [md, tmpl]
    --   cmd "pandoc" md "--to html --filter tools/pandocRemoveManpageBlocks.hs --template" tmpl "-o" out

    -- "site/manual2.html" %> \out -> do
    --   need ["site/manual2.md"]
    --   cmd "pandoc site/manual2.md -o" out

    -- "_build//*.o" %> \out -> do
    --     let c = dropDirectory1 $ out -<.> "c"
    --     let m = out -<.> "m"
    --     () <- cmd "gcc -c" [c] "-o" [out] "-MMD -MF" [m]
    --     needMakefileDependencies m
