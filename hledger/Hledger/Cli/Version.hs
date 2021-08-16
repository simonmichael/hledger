{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-
Version number-related utilities. See also the Makefile.
-}

module Hledger.Cli.Version (
  packageversion,
  progname,
  prognameandversion,
  versionStringFor,
)
where

import GitHash (giDescribe, tGitInfoCwdTry)
import System.Info (os, arch)
import Hledger.Utils

-- | This package's version, passed in as VERSION build variable, or a generic description.
packageversion :: String
#ifdef VERSION
packageversion = VERSION
#else
packageversion = "dev build"
#endif

-- | A period and the patch level (number of patches added since the package version), 
-- passed in as PATCHLEVEL build variable, or the empty string. 
-- If PATCHLEVEL is defined it must be a number, or this will fail.
patchlevel :: String
#ifdef PATCHLEVEL
patchlevel = "." ++ show (PATCHLEVEL :: Int)
#else
patchlevel = ""
#endif

-- | The version and patchlevel passed in as build variables, combined and prettified.
-- This will raise an error if VERSION is has <1 or >3 components,
-- or if PATCHLEVEL is defined but not a number.
-- Used as a fallback if git describe is unavailable.
buildversion :: String
buildversion = prettify . splitAtElement '.' $ packageversion ++ patchlevel
  where
    prettify [major,minor,bugfix,patches] =
        major ++ "." ++ minor ++ bugfix' ++ patches'
      where
        bugfix'  = if bugfix  == "0" then "" else '.' : bugfix
        patches' = if patches == "0" then "" else '+' : patches
    prettify [major,minor,bugfix] = prettify [major,minor,bugfix,"0"]
    prettify [major,minor]        = prettify [major,minor,"0","0"]
    prettify [major]              = prettify [major,"0","0","0"]
    prettify []                   = error' "VERSION is empty, please fix"  -- PARTIAL:
    prettify _                    = error' "VERSION has too many components, please fix"

-- | The name of this package's main executable.
progname :: String
progname = "hledger"

-- | The program name and the best version information we can obtain 
-- from git describe or build variables.
prognameandversion :: String
prognameandversion = versionStringFor progname

-- | Given a program name, make a version string consisting of: 
--
-- * the program name
-- * the output of "git describe" in the current repo at build time 
--   (last tag, commit count since then, HEAD's git hash);
--   or if that fails, buildversion
-- * the platform (OS) name
-- * the processor architecture name.
--
versionStringFor :: String -> String
versionStringFor progname = concat [
    progname
  , " "
  , either (const buildversion) giDescribe $$tGitInfoCwdTry
  , ", "
  , os'
  , "-"
  , arch
  ]
  where
    os' | os == "darwin"  = "mac"
        | os == "mingw32" = "windows"
        | otherwise       = os

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
