{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-
Version number-related utilities. See also the Makefile.
-}

module Hledger.Cli.Version (
  progname,
  version,
  prognameandversion,
  versiondescription,
  binaryfilename,
)
where

#if MIN_VERSION_githash(0,1,4)
import GitHash (giDescribe, tGitInfoCwdTry)
#else
import GitHash (giHash, tGitInfoCwdTry)
#endif
import System.Info (os, arch)
import Hledger.Utils


-- package name and version from the cabal file
progname, version, prognameandversion :: String
progname = "hledger"
#ifdef VERSION
version = VERSION
#else
version = "dev build"
#endif
prognameandversion = versiondescription progname

-- developer build version strings include PATCHLEVEL (number of
-- patches since the last tag). If defined, it must be a number.
patchlevel :: String
#ifdef PATCHLEVEL
patchlevel = "." ++ show (PATCHLEVEL :: Int)
#else
patchlevel = ""
#endif

-- the package version plus patchlevel if specified
buildversion :: String
buildversion = prettify . splitAtElement '.' $ version ++ patchlevel
  where
    prettify (major:minor:bugfix:patches:[]) =
        major ++ "." ++ minor ++ bugfix' ++ patches'
      where
        bugfix'  = if bugfix  == "0" then "" else '.' : bugfix
        patches' = if patches == "0" then "" else '+' : patches
    prettify (major:minor:bugfix:[]) = prettify [major,minor,bugfix,"0"]
    prettify (major:minor:[])        = prettify [major,minor,"0","0"]
    prettify (major:[])              = prettify [major,"0","0","0"]
    prettify []                      = error' "VERSION is empty, please fix"  -- PARTIAL:
    prettify _                       = error' "VERSION has too many components, please fix"

-- | A string representing the version description of the current package
versiondescription :: String -> String
versiondescription progname = concat
#if MIN_VERSION_githash(0,1,4)
    [progname, " ", either (const buildversion) giDescribe gi, ", ", os', "-", arch]
#else
    [progname, " ", buildversion, either (const "") (\x -> ", git revision " ++ giHash x) gi, ", ", os', "-", arch]
#endif
  where
    gi = $$tGitInfoCwdTry
    os' | os == "darwin"  = "mac"
        | os == "mingw32" = "windows"
        | otherwise       = os

-- | Given a program name, return a precise platform-specific executable
-- name suitable for naming downloadable binaries.  Can raise an error if
-- the version and patch level was not defined correctly at build time.
binaryfilename :: String -> String
binaryfilename progname = concat
    [progname, "-", buildversion, "-", os', "-", arch, suffix]
  where
    (os',suffix) | os == "darwin"  = ("mac","" :: String)
                 | os == "mingw32" = ("windows",".exe")
                 | otherwise       = (os,"")
