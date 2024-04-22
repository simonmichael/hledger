#!/usr/bin/env runhaskell
{-
Make/update hledger release notes from the latest release's changelogs.
Run this in the root of the hledger repo, in a release branch.
If you have switched branch recently you may have to kill an unhelpful
ghc environment file first. Eg: rm -f .ghc.environment* && tools/relnotes.hs

This reads the latest release's changes from the main CHANGES.md files,
converts to release notes format, and inserts this into relnotes.md.
If there already were release notes for this release (at top of relnotes),
they will be replaced, except for a manually added "highlights" paragraph if any
(after the release heading, beginning and ending with **), which is preserved.

The release's version is taken from the the first h1 heading in the project CHANGES.md.
It is assumed that all the other changelogs' first section is also for this version (XXX),
and that changelogs and relnotes are non-empty with specific layout and heading formats.

In the end I wrote this in haskell because everything else was harder.
-}

import Data.Char
import Data.List
import System.IO
import System.Process
import Text.Printf

main = do
  -- gather latest release changes & info
  (projectChangesHeading, projectChanges)       <- changelogFirstSection <$> readFile "CHANGES.md"
  (hledgerChangesHeading, hledgerChanges)       <- changelogFirstSection <$> readFile "hledger/CHANGES.md"
  (hledgerUiChangesHeading, hledgerUiChanges)   <- changelogFirstSection <$> readFile "hledger-ui/CHANGES.md"
  (hledgerWebChangesHeading, hledgerWebChanges) <- changelogFirstSection <$> readFile "hledger-web/CHANGES.md"
  reltags <- lines <$> readProcess "git" ["tag", "--sort=-creatordate", "-l", "[0-9]*"] ""
  let
    [_, ver, date] = words projectChangesHeading
    prevver:_ = drop 1 $ dropWhile (/=ver) reltags
  relauthors <- map (unwords . drop 1 . words) . lines <$> readProcess "git" ["shortlog", "-sn", prevver<>".."<>ver] ""

  -- convert to release notes format
  let
    newrelnotesheading = printf "## %s hledger-%s\n" date ver
    newrelnotesbody = intercalate "\n\n" [
      changelogHeadingToRelnotesHeading "hledger" hledgerChangesHeading,        hledgerChanges,
      changelogHeadingToRelnotesHeading "hledger-ui" hledgerUiChangesHeading,   hledgerUiChanges,
      changelogHeadingToRelnotesHeading "hledger-web" hledgerWebChangesHeading, hledgerWebChanges,
      "### project changes " <> ver <> "\n",                                    projectChanges,
      "### credits " <> ver <> "\n",                                            intercalate ",\n" relauthors <> ".\n"
      ] <> "\n\n"

  -- insert or update in relnotes.md
  let relnotesfile = "doc/relnotes.md"
  (preamble, rnlatestver, rnlatesthighlights, rnlatestbody, rest) <-
    relnotesSections <$> readFile' relnotesfile
  -- putStrLn $
  writeFile relnotesfile $
    unlines [
      preamble,
      newrelnotesheading,
      if null rnlatesthighlights then "" else rnlatesthighlights <> "\n",
      newrelnotesbody,
      if rnlatestver == ver then "" else rnlatestbody,
      rest
      ] <> "\n"

changelogFirstSection alltext =
  case uncons $ dropWhile (not.isReleaseHeading) $ lines alltext of
    Nothing     -> ("","")
    Just (l,ls) -> (nl l, strip' $ unlines $ takeWhile (not.isReleaseHeading) ls)
  where
    isReleaseHeading = isPrefixOf "# "

changelogHeadingToRelnotesHeading pkgname heading =
  let [_, ver, _] = words heading
  in printf "### %s %s\n" pkgname ver

relnotesSections alltext = (unlines preamble, firstsectionver, firstsectionhighlights, firstsectionbody, rest)
  where
    isReleaseHeading = isPrefixOf "## "
    (preamble, (firstsectionheading:ls)) = span (not.isReleaseHeading) $ lines alltext
    firstsectionver = drop 8 $ (!!2) $ words firstsectionheading
    ls2 = dropWhile null ls
    (firstsectionhighlights, ls3) =
      case ls2 of
        ('*':'*':_):_ -> (unlines $ takeWhile (not.null) ls2, dropWhile (not.null) ls2)
        _             -> ("", ls2)
    (firstsectionls, restls) = span (not.isReleaseHeading) ls3
    firstsectionbody = unlines $ firstsectionheading : firstsectionls
    rest = unlines restls

unlines' = intercalate "\n"

lstrip = dropWhile isSpace
rstrip = reverse . lstrip . reverse
strip = lstrip . rstrip
strip' = nl . strip
nl = (<>"\n")
