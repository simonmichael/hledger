{-# LANGUAGE CPP, TemplateHaskell #-}
{-
Version number-related utilities. See also the Makefile.
-}

module Hledger.Cli.Version (
  progname,
  version,
  prognameandversion,
  binaryfilename
)
where
import Data.List
import Distribution.PackageDescription.TH (packageVariable, package, pkgName, pkgVersion)
import System.Info (os, arch)
import Text.Printf

import Hledger.Utils


-- package name and version from the cabal file
progname, version, prognameandversion :: String
progname = $(packageVariable (pkgName . package))
version  = $(packageVariable (pkgVersion . package))
prognameandversion = progname ++ " " ++ version

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
buildversion  = version ++ patchlevel

-- | Given a program name, return a precise platform-specific executable
-- name suitable for naming downloadable binaries.  Can raise an error if
-- the version and patch level was not defined correctly at build time.
binaryfilename :: String -> String
binaryfilename progname = prettify $ splitAtElement '.' buildversion
                where
                  prettify (major:minor:bugfix:patches:[]) =
                      printf "%s-%s.%s%s%s-%s-%s%s" progname major minor bugfix' patches' os' arch suffix
                          where
                            bugfix'
                                | bugfix `elem` ["0"{-,"98","99"-}] = ""
                                | otherwise = '.' : bugfix
                            patches'
                                | patches/="0" = '+' : patches
                                | otherwise = ""
                            (os',suffix)
                                | os == "darwin"  = ("mac","")
                                | os == "mingw32" = ("windows",".exe")
                                | otherwise       = (os,"")
                  prettify (major:minor:bugfix:[]) = prettify [major,minor,bugfix,"0"]
                  prettify (major:minor:[])        = prettify [major,minor,"0","0"]
                  prettify (major:[])              = prettify [major,"0","0","0"]
                  prettify []                      = error' "VERSION is empty, please fix"
                  prettify _                       = error' "VERSION has too many components, please fix"
