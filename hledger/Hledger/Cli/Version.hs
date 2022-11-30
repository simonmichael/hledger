{-# LANGUAGE CPP             #-}
{-
Version number-related utilities. See also the Makefile.
-}

module Hledger.Cli.Version (
  ProgramName,
  PackageVersion,
  VersionString,
  packageversion,
  progname,
  versionStringWith,
)
where

import GitHash (GitInfo, giHash, giCommitDate)  -- giDirty
import System.Info (os, arch)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

type ProgramName    = String
type PackageVersion = String
type VersionString  = String

-- | The VERSION string defined with -D in this package's package.yaml/.cabal file 
-- (by Shake setversion), if any. Normally a dotted number string with 1-3 components.
packageversion :: PackageVersion
packageversion =
#ifdef VERSION
  VERSION
#else
  ""
#endif

-- | The name of this package's main executable.
progname :: ProgramName
progname = "hledger"

-- | Given possible git state info from the build directory (or an error message, which is ignored),
-- the name of a program (executable) in the currently building package,
-- and the package's version, make a complete version string. Here is the logic:
-- 
-- * Program name, OS and architecture are always shown.
-- * The package version is always shown.
-- * If there is git info at build time, the latest commit hash and commit date are shown,
--   and (TODO, requires githash to use -uno for giDirty):
--   if the working copy has uncommitted changes a + sign is appended.
-- * (TODO, requires adding --match support to githash:
--   If there are tags matching THISPKG-[0-9]*, the latest one is used to calculate patch level
--   (number of commits since tag), and if non-zero, it and the branch name are shown.)
--
-- Some example outputs:
--
-- * A homebrew binary, not built in git repo:             hledger-ui 1.24, mac-aarch64
-- * A CI release build, built in git repo at release tag: hledger-ui 1.24.1-g455b35293-20211210, mac-x86_64
-- * (TODO) A dev build, built in git repo:                hledger-ui 1.24.1+1-g4abd8ef10-20211210 (1.24-branch), mac-x86_64
--
-- This function requires git log to show the default (rfc2822-style) date format,
-- so that must not be overridden by a log.date git config variable.
--
versionStringWith :: Either String GitInfo -> ProgramName -> PackageVersion -> VersionString
versionStringWith egitinfo prognam packagever =
  concat [ prognam , " " , version , ", " , os' , "-" , arch ]
  where
    os' | os == "darwin"  = "mac"
        | os == "mingw32" = "windows"
        | otherwise       = os
    version = case egitinfo of
      Left _err     -> packagever
      Right gitinfo -> 
        case words $ giCommitDate gitinfo of
          -- git log's date format is normally --date=default ("similar to --date=rfc2822")
          _weekday:mon:day:_localtime:year:_offset:_ ->
            intercalate "-" $ [packagever, hash, date]
              -- ++ ["+" | giDirty gitinfo]
              --   XXX giDirty is wrong when repo shows untracked files by default, skip it for now
              where
                hash = 'g' : take 9 (giHash gitinfo)  -- like git describe
                date = concat [year,mm,dd]
                  where 
                    mm = fromMaybe mon $ lookup mon $ [
                      ("Jan","01")
                      ,("Feb","02")
                      ,("Mar","03")
                      ,("Apr","04")
                      ,("May","05")
                      ,("Jun","06")
                      ,("Jul","07")
                      ,("Aug","08")
                      ,("Sep","09")
                      ,("Oct","10")
                      ,("Nov","11")
                      ,("Dec","12")
                      ]
                    dd = (if length day < 2 then ('0':) else id) day
          -- but could be overridden by a log.date config variable in repo or user git config
          _ -> packageversion

-- -- | Given a program name, return a precise platform-specific executable
-- -- name suitable for naming downloadable binaries.  Can raise an error if
-- -- the version and patch level was not defined correctly at build time.
-- binaryfilename :: String -> String
-- binaryfilename progname = concat
--     [progname, "-", buildversion, "-", os', "-", arch, suffix]
--   where
--     (os',suffix) | os == "darwin"  = ("mac","" :: String)
--                  | os == "mingw32" = ("windows",".exe")
--                  | otherwise       = (os,"")
