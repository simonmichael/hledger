#!/usr/bin/env stack
-- stack runghc --package shake

import Development.Shake
import Development.Shake.FilePath
import Data.List
import System.Directory as S (getDirectoryContents)

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

main :: IO ()
main = do

  pandocFilters <-
    map ("tools" </>). nub . sort . map (-<.> "") . filter ("pandoc" `isPrefixOf`)
    <$> S.getDirectoryContents "tools"

  -- man pages, still markdown but with man-only sections removed.
  -- (We let hakyll do the html rendering since it's good
  -- at applying the site style, table of contents etc.)
  let manpageFilteredMds = ["site" </> p <.>".md" | p <- manpages]

  -- man pages, converted to man nroff with web-only sections removed
  let manpageNroffs = [manpageDir p </> p | p <- manpages]

  shakeArgs shakeOptions{shakeFiles=buildDir} $ do

    want $ manpageNroffs ++ manpageFilteredMds

    manpageNroffs |%> \out -> do
      let
        md = out <.> "md"
        tmpl = "doc/manpage.nroff"
      need $ md : tmpl : pandocFilters
      cmd "pandoc" md "--to man -s --template" tmpl
        -- XXX assume these are compiled
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
      cmd "stack ghc" out

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
